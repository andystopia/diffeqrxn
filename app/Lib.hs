{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import AST
import qualified CodeWriter as CW
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Either (lefts)
import Data.Foldable (fold)
import qualified Data.Text as T
import Effectful
import qualified Error.Diagnose as Diag
import ErrorProvider
import Exporters (asciiExporter, juliaExporter, typstExporter)
import Lexer
import Parser
import Utils

data WasmProgramResult = WasmSuccess {jsonlints :: String, jsonoutput :: T.Text } | WasmFailure {jsonerrors :: String } deriving (Show)

instance A.ToJSON WasmProgramResult where
  toJSON (WasmSuccess jl jo) = A.object ["status" .= ("success" :: String), "lints" .= jl, "output" .= jo]
  toJSON (WasmFailure jo) = A.object ["status" .= ("failure" :: String), "output" .= show jo]


stringToExportFormat :: String -> Maybe ExportFormat 
stringToExportFormat "Julia" = Just JuliaExport
stringToExportFormat "ASCII" = Just AsciiExport
stringToExportFormat "Typst" = Just TypstExport
stringToExportFormat _ = Nothing

data ProgramInput = ProgramInput
  { inputModelName :: T.Text,
    inputModelFilename :: T.Text,
    inputModelText :: T.Text,
    inputParameterLookup :: T.Text -> Maybe T.Text,
    inputExportFormat :: ExportFormat
  }

runWithInput :: (SemanticErrorEff :> es) => ProgramInput -> (Eff es) (Diag.Diagnostic String, T.Text)
runWithInput programInput = do
  let filePath = T.unpack $ inputModelFilename programInput
  let text = T.unpack $ inputModelText programInput

  semAddFile filePath text
  semActivateFile filePath

  -- the lexer error handling is unfortunatly slightly
  -- more complicated than it otherwise would be here
  -- simply because the MegaParsec library already handles
  -- conversion into our error types, so instead we simply
  -- handle the error over here
  lexed <- lexer filePath text

  (variableQualifiers, modelEquations) <- parse lexed

  lints <- fold . lefts <$> traverse (semSubscope . lintEquation) modelEquations

  -- convert them into "normal form"
  let normalForms = equationsToNormalForm modelEquations

  -- take the equations that are in normal form
  -- and resolve and expand their macros.
  resolvedNotQualified <- resolveEquations basicMacroProvider normalForms

  let resolved = qualifyResolvedVariables variableQualifiers resolvedNotQualified

  -- just a convenience helper function for running a model exporter
  let runExporter model = T.pack $ CW.evalStringCodeWriter 4 (model (T.unpack $ inputModelName programInput) resolved)

  return
    ( lints,
      case inputExportFormat programInput of
        AsciiExport -> runExporter asciiExporter
        JuliaExport -> T.pack $ CW.evalStringCodeWriter 4 (juliaExporter (T.unpack $ inputModelName programInput) resolved (inputParameterLookup programInput))
        TypstExport -> runExporter typstExporter
    )
