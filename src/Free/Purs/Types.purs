module Free.Purs.Types where

import Control.Monad.Eff (kind Effect)

import Data.StrMap as StrMap

import PscIde.Command as Ide

import Unsafe.Coerce (unsafeCoerce)

class Subrow (r :: # Type) (s :: # Type)

instance subrow :: Union r t s => Subrow r s

foreign import data PURS :: Effect

type PursEffect eff = (purs :: PURS | eff)

type Source = String

type Port = Int

data IdeLogLevel = IdeLogDebug | IdeLogPerf | IdeLogAll | IdeLogNone | IdeLogDefault

type IdeClientCommand = Ide.Command

newtype IdeServerResult = IdeServerResult (forall a. a)

type CompileResult = { stdout :: String, stderr :: String }

type BundleResult = CompileResult

data IdeClientResult
  = IdeMessageResult Ide.Message
  | IdeLsModulesResult Ide.ModuleList
  | IdeLsImportsResult Ide.ImportList
  | IdePursuitResult (Array Ide.PursuitCompletion)
  | IdeTypeInfoArrayResult (Array Ide.TypeInfo)
  | IdeStringArrayResult (Array String)
  | IdeImportResult Ide.ImportResult
  | IdeRebuildResult Ide.RebuildResult

type CompileOption r
  = ( output :: String
    , verboseErrors :: Boolean
    , comments :: Boolean
    , sourceMaps :: Boolean
    , dumpCoreFn :: Boolean
    , noPrefix :: Boolean
    , jsonErrors :: Boolean
    | r
    )

type BundleOption r
  = ( output :: String
    , module :: Array String
    , main :: String
    , namespace :: String
    , sourceMaps :: Boolean
    | r
    )

type IdeServerOption r
  = ( directory :: String
    , sources :: Array String
    , outputDirectory :: String
    , port :: Port
    , noWatch :: Boolean
    , polling :: Boolean
    , logLevel :: IdeLogLevel
    | r
    )

type IdeClientOption r
  = ( port :: Port
    | r
    )

mergeRecords :: forall r s t. Union r s t => Record r -> Record s -> Record t
mergeRecords r s = unsafeCoerce (StrMap.union (unsafeCoerce r) (unsafeCoerce s))

dedupeRows :: forall r s. Record r -> Record s
dedupeRows = unsafeCoerce
