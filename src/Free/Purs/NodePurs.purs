module Free.Purs.NodePurs
  ( NodePursOption
  , NodePursEffect
  , nodePurs
  ) where

import Prelude

import Control.Monad.Aff (Aff, Canceler(..), attempt, cancel, makeAff, forkAff, cancelWith)
import Control.Monad.Aff.AVar (AVAR, makeVar, takeVar, putVar)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Ref (REF, newRef, modifyRef, readRef)
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Posix.Signal as Posix

import Free.Purs (PursF(..))
import Free.Purs.Types
  ( class Subrow
  , BundleOption
  , CompileOption
  , IdeServerOption
  , IdeServerResult(..)
  , IdeClientOption
  , IdeClientResult(..)
  , IdeLogLevel(..)
  , PursEffect
  , mergeRecords
  )

import Node.ChildProcess (ChildProcess, CHILD_PROCESS, Exit(..))
import Node.ChildProcess (stdout, stderr, stdin, onExit, onError, toStandardError, kill) as Node
import Node.Encoding (Encoding(UTF8))
import Node.Stream (Writable)
import Node.Stream (onDataString, writeString) as Node

import PscIde.Command as Ide

import Unsafe.Coerce (unsafeCoerce)

type NodePursOption r
  = ( compileCommand :: String
    , compileArgs :: Array String
    , bundleCommand :: String
    , bundleArgs :: Array String
    , ideServerCommand :: String
    , ideServerArgs :: Array String
    , ideClientCommand :: String
    , ideClientArgs :: Array String
    | r
    )

type NodePursEffect eff = PursEffect (avar :: AVAR, console :: CONSOLE, cp :: CHILD_PROCESS, exception :: EXCEPTION, ref :: REF | eff)

nodePurs :: forall eff r s. Union r (NodePursOption ()) (NodePursOption s) => Subrow s (NodePursOption ()) => Record r -> PursF ~> Aff (NodePursEffect eff)
nodePurs pursOptions =
  case _ of
       Compile sources options k -> do
         let cmd = pursOptions'.compileCommand

             args = pursOptions'.compileArgs <> sources <> compileOptions options

             input = Nothing

         k <$> spawn cmd args input

       Bundle source options k -> do
         let cmd = pursOptions'.bundleCommand

             args = pursOptions'.bundleArgs <> [ source ] <> bundleOptions options

             input = Nothing

         k <$> spawn cmd args input

       IdeServer options k -> do
         let cmd = pursOptions'.ideServerCommand

             args = pursOptions'.ideServerArgs <> ideServerOptions options

             input = Nothing

         k <<< toIdeServerResult <$> forkAff (spawn cmd args input)

       IdeClient command options k -> do
         let cmd = pursOptions'.ideClientCommand

             args = pursOptions'.ideClientArgs <> ideClientOptions options

             input = Just (stringify (encodeJson command))

         { stdout } <- spawn cmd args input

         result <- case command of
                        Ide.Cwd -> IdeMessageResult <$> decodeResult stdout
                        Ide.Ls (Ide.Imports _) -> IdeLsImportsResult <$> decodeResult stdout
                        Ide.Ls _ -> IdeLsModulesResult <$> decodeResult stdout
                        Ide.Quit -> IdeMessageResult <$> decodeResult stdout
                        Ide.Reset -> IdeMessageResult <$> decodeResult stdout
                        Ide.Load _ _ -> IdeMessageResult <$> decodeResult stdout
                        Ide.Complete _ _ _ -> IdeTypeInfoArrayResult <$> decodeResult stdout
                        Ide.Pursuit _ _ -> IdePursuitResult <$> decodeResult stdout
                        Ide.Type _ _ _ -> IdeTypeInfoArrayResult <$> decodeResult stdout
                        Ide.AddClause _ _ -> IdeStringArrayResult <$> decodeResult stdout
                        Ide.CaseSplit _ _ _ _ _ -> IdeStringArrayResult <$> decodeResult stdout
                        Ide.ImportCmd _ _ _ _ -> IdeImportResult <$> decodeResult stdout
                        Ide.RebuildCmd _ -> IdeRebuildResult <$> decodeResult stdout

         pure (k result)

       StopIdeServer (IdeServerResult canceler) a ->
         a <$ cancel canceler (Exception.error "Stop the ide server")
  where
  decodeResult :: forall a. DecodeJson a => String -> Aff (NodePursEffect eff) a
  decodeResult = either (throwError <<< Exception.error) pure <<< join <<< Ide.unwrapResponse

  toIdeServerResult :: Canceler (NodePursEffect eff) -> IdeServerResult
  toIdeServerResult = IdeServerResult <<< unsafeCoerce

  pursOptions' :: Record (NodePursOption s)
  pursOptions' = mergeRecords pursOptions pursDefaults

  pursDefaults :: Record (NodePursOption ())
  pursDefaults =
    { compileCommand: "purs"
    , compileArgs:
        [ "compile"
        ]
    , bundleCommand: "purs"
    , bundleArgs:
        [ "bundle"
        ]
    , ideServerCommand: "purs"
    , ideServerArgs:
        [ "ide"
        , "server"
        ]
    , ideClientCommand: "purs"
    , ideClientArgs:
        [ "ide"
        , "client"
        ]
    }

  compileOptions :: forall t. Record (CompileOption t) -> Array String
  compileOptions opts =
    if opts.output /= mempty
       then [ "--output", opts.output ]
       else [ ] <>
    if opts.verboseErrors
       then [ "--verbose-errors" ]
       else [ ] <>
    if opts.comments
       then [ "--comments" ]
       else [ ] <>
    if opts.sourceMaps
       then [ "--source-maps" ]
       else [ ] <>
    if opts.dumpCoreFn
       then [ "--dump-corefn" ]
       else [ ] <>
    if opts.noPrefix
       then [ "--no-prefix" ]
       else [ ] <>
    if opts.jsonErrors
       then [ "--json-errors" ]
       else [ ]

  bundleOptions :: forall t. Record (BundleOption t) -> Array String
  bundleOptions opts =
    join (append [ "--module" ] <<< pure <$> opts.module) <>
    if opts.namespace /= mempty
       then [ "--namespace", opts.namespace ]
       else [ ] <>
    if opts.output /= mempty
       then [ "--output", opts.output ]
       else [ ] <>
    if opts.main /= mempty
       then [ "--main", opts.main ]
       else [ ] <>
    if opts.sourceMaps
       then [ "--source-maps" ]
       else [ ]

  ideServerOptions :: forall t. Record (IdeServerOption t) -> Array String
  ideServerOptions opts =
    opts.sources <>
    [ "--port", show opts.port ] <>
    case opts.logLevel of
         IdeLogDebug ->
         [ "--log-level", "debug" ]
         IdeLogPerf ->
         [ "--log-level", "perf" ]
         IdeLogAll ->
         [ "--log-level", "all" ]
         IdeLogNone ->
         [ "--log-level", "none" ]
         IdeLogDefault ->
         [ ]
    <>
    if opts.directory /= mempty
       then [ "--directory", opts.directory ]
       else [ ] <>
    if opts.outputDirectory /= mempty
       then [ "--output-directory", opts.outputDirectory ]
       else [ ] <>
    if opts.noWatch
       then [ "--no-watch" ]
       else [ ] <>
    if opts.polling
       then [ "--polling" ]
       else [ ]

  ideClientOptions :: forall t. Record (IdeClientOption t) -> Array String
  ideClientOptions opts =
    [ "--port", show opts.port
    ]

spawn :: forall eff. String -> Array String -> Maybe String -> Aff (NodePursEffect eff) { stdout :: String, stderr :: String }
spawn cmd args input = do
  liftEff (debug "%s %o" [ cmd, unsafeCoerce args ])

  child <- liftEff (execa cmd args)

  let stdout = Node.stdout child

      stderr = Node.stderr child

      stdin = Node.stdin child

  stdoutRef <- liftEff (newRef "")

  stderrRef <- liftEff (newRef "")

  liftEff (Node.onDataString stdout UTF8 (modifyRef stdoutRef <<< (flip append)))

  liftEff (Node.onDataString stderr UTF8 (modifyRef stderrRef <<< (flip append)))

  liftEff (debug "stdin %o" [ unsafeCoerce input ])

  maybe (pure unit) (onInput stdin) input

  latch <- makeVar

  onErrorCancel <- forkAff (attempt (onError child) >>= putVar latch)

  onExitCancel <- forkAff (attempt (onExit child) >>= putVar latch)

  result <- cancelWith (takeVar latch) (Canceler (const (liftEff (Node.kill Posix.SIGTERM child))))

  stdoutValue <- liftEff (readRef stdoutRef)

  stderrValue <- liftEff (readRef stderrRef)

  liftEff (debug "stdout %s" [ stdoutValue ])

  liftEff (debug "stderr %s" [ stderrValue ])

  either (const (throwError (Exception.error stderrValue)))
         (const (pure { stdout: stdoutValue
                      , stderr: stderrValue
                      }))
         result
  where
  onInput :: forall t. Writable t (NodePursEffect eff) -> String -> Aff (NodePursEffect eff) Unit
  onInput stdin input' = makeAff \_ callback -> void (Node.writeString stdin UTF8 (input' <> "\n") (callback unit))

  onError :: ChildProcess -> Aff (NodePursEffect eff) Unit
  onError child = makeAff \errback _ -> Node.onError child (errback <<< Node.toStandardError)

  onExit :: ChildProcess -> Aff (NodePursEffect eff) Unit
  onExit child = makeAff \errback callback -> Node.onExit child
    case _ of
         Normally 0 -> callback unit
         exit -> errback (Exception.error (show exit))

foreign import execa :: forall eff. String -> Array String -> Eff (cp :: CHILD_PROCESS | eff) ChildProcess

foreign import debug :: forall eff value. String -> Array value -> Eff (console :: CONSOLE | eff) Unit
