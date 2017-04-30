module Free.Purs
  ( Purs
  , PursF(..)
  , compile
  , bundle
  , ideServer
  , ideClient
  , stopIdeServer
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)

import Data.Monoid (mempty)

import Free.Purs.Types
  ( class Subrow
  , Source
  , IdeLogLevel(..)
  , IdeServerResult
  , IdeClientCommand
  , IdeClientResult
  , CompileOption
  , CompileResult
  , BundleOption
  , BundleResult
  , IdeServerOption
  , IdeClientOption
  , dedupeRows
  , mergeRecords
  )

type Purs a = Free PursF a

data PursF a
  = Compile (Array Source) (Record (CompileOption ())) (CompileResult -> a)
  | Bundle Source (Record (BundleOption ())) (BundleResult -> a)
  | IdeServer (Record (IdeServerOption ())) (IdeServerResult -> a)
  | IdeClient IdeClientCommand (Record (IdeClientOption ())) (IdeClientResult -> a)
  | StopIdeServer IdeServerResult a

compile :: forall r s. Union r (CompileOption ()) (CompileOption s) => Subrow s (CompileOption ()) => Array Source -> Record r -> Purs CompileResult
compile sources options = liftF (Compile sources (dedupeRows options') id)
  where
  options' :: Record (CompileOption s)
  options' = mergeRecords options defaults

  defaults :: Record (CompileOption ())
  defaults =
    { output: mempty
    , verboseErrors: false
    , comments: false
    , sourceMaps: false
    , dumpCoreFn: false
    , noPrefix: false
    , jsonErrors: false
    }

bundle :: forall r s. Union r (BundleOption ()) (BundleOption s) => Subrow s (BundleOption ()) => Source -> Record r -> Purs BundleResult
bundle source options = liftF (Bundle source (dedupeRows options') id)
  where
  options' :: Record (BundleOption s)
  options' = mergeRecords options defaults

  defaults :: Record (BundleOption ())
  defaults =
    { output: mempty
    , module: mempty
    , main: mempty
    , namespace: mempty
    , sourceMaps: false
    }

ideServer :: forall r s. Union r (IdeServerOption ()) (IdeServerOption s) => Subrow s (IdeServerOption ()) => Record r -> Purs IdeServerResult
ideServer options = liftF (IdeServer (dedupeRows options') id)
  where
  options' :: Record (IdeServerOption s)
  options' = mergeRecords options defaults

  defaults :: Record (IdeServerOption ())
  defaults =
    { directory: mempty
    , sources: mempty
    , outputDirectory: mempty
    , port: 4242
    , noWatch: false
    , polling: false
    , logLevel: IdeLogDefault
    }

ideClient :: forall r s. Union r (IdeClientOption ()) (IdeClientOption s) => Subrow s (IdeClientOption ()) => IdeClientCommand -> Record r -> Purs IdeClientResult
ideClient source options = liftF (IdeClient source (dedupeRows options') id)
  where
  options' :: Record (IdeClientOption s)
  options' = mergeRecords options defaults

  defaults :: Record (IdeClientOption ())
  defaults =
    { port: 4242
    }

stopIdeServer :: IdeServerResult -> Purs Unit
stopIdeServer result = liftF (StopIdeServer result unit)
