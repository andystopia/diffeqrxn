{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import AST
import qualified Cli
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Error.Diagnose as Diag
import Lib
import System.IO (stderr)
import TomlParamProvider (getDefaultVariableLUT)
import Prelude hiding (lex)

main :: IO ()
main = do
  opt <- Cli.cli

  case opt of
    Cli.SubcommandExport
      (Cli.ExportOpts filePath modelName exportFormat paramsFile) -> do
        -- read in the file that we're loading the equations from
        fileContents <- readFile filePath
        paramFileContents <- TIO.readFile `traverse` paramsFile

        -- now run the error recording effect, and parse the
        -- parameters file from TOML to something we can actually use
        let res = semanticErrorEffToEither $ do
              paramGetter <- getDefaultVariableLUT modelName `traverse` paramFileContents

              let paramProvider = fromMaybe (const Nothing) paramGetter

              runWithInput $
                ProgramInput
                  { inputModelName = modelName,
                    inputModelFilename = T.pack filePath,
                    inputModelText = T.pack fileContents,
                    inputParameterLookup = paramProvider,
                    inputExportFormat = exportFormat
                  }

        case res of
          Left err -> do
            void $ Diag.printDiagnostic stderr Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle err
          Right (lints, success) -> do
            Diag.printDiagnostic stderr Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle lints
            putStrLn (T.unpack success)
