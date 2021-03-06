module Test.Main (main) where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)

import Data.Either (Either, either)
import Data.Newtype (wrap)
import Data.String (take, length)
import Data.String.Regex (Regex)
import Data.String.Regex (regex, test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex

import Free.Purs (Purs, PursF)
import Free.Purs.Types (IdeClientResult(..))
import Free.Purs (compile, bundle, ideServer, ideClient, stopIdeServer) as Purs
import Free.Purs.NodePurs (NodePursEffect, nodePurs)

import PscIde.Command as Ide

import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, Config, run', defaultConfig)

main :: Eff (NodePursEffect (RunnerEffects ())) Unit
main = run' config [ consoleReporter ] do
  describe "compile" do
    it "should produce output" do
      let program = Purs.compile [ "bower_components/purescript-*/src/**/*.purs", "src/**/*.purs", "test/**/*.purs" ] { output }

      { stdout } <- runProgram program

      stdout `shouldMatch` Regex.regex "(^$)|(Compiling)" Regex.noFlags

  describe "bundle" do
    it "should produce a bundle" do
      let program = Purs.bundle (output <> "/**/*.js") { module: [ "Free.Purs.Types" ] }

          expected = "// Generated by"

      { stdout } <- runProgram program

      let stdout' = take (length expected) stdout

      stdout' `shouldMatch` Regex.regex expected Regex.noFlags

  describe "ideServer" do
    it "should stop the server" do
      cancel <- runProgram (Purs.ideServer { port })

      delay (wrap 10000.0)

      runProgram (Purs.stopIdeServer cancel)

      pure unit

  describe "ideClient" do
    it "should rebuild" do
      cancel <- runProgram (Purs.ideServer { port })

      delay (wrap 10000.0)

      let load = Ide.Load [] []

          rebuild = Ide.RebuildCmd "src/Free/Purs/Types.purs"

          program = do _ <- Purs.ideClient load { port }
                       result <- Purs.ideClient rebuild { port }
                       Purs.stopIdeServer cancel
                       pure result

      result <- runProgram program

      let result' = case result of
                         IdeRebuildResult (Ide.RebuildResult []) -> true
                         _ -> false

      result' `shouldEqual` true
  where
  output :: String
  output = "output-test"

  port :: Int
  port = 8234

  config :: Config
  config = defaultConfig { timeout = pure 60000 }

shouldMatch :: forall eff. String -> Either String Regex -> Aff eff Unit
shouldMatch v re = do
  re' <- either (throwError <<< error) pure re
  when (not $ Regex.test re' v) $
    fail $ show v <> " does not match " <> show re

runProgram :: forall eff a. Purs a -> Aff (NodePursEffect eff) a
runProgram = foldFree interpreter
  where
  interpreter :: PursF ~> Aff (NodePursEffect eff)
  interpreter = nodePurs {}
