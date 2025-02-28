{-# LANGUAGE CPP #-}

module WasmBind where


#ifdef WASM
import AST
import Data.Maybe (fromJust)
import qualified Data.Aeson as A
import Data.ByteString.Lazy.Char8 as BL
import Data.Char (ord)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import GHC.Wasm.Prim
import Lib
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String
import Utils

-- # Forger and Peskin 2003: PNAS
jsExportFormats :: JSString
jsExportFormats = toJSString "[\"Julia\", \"Typst\", \"ASCII\"]"

jsRunModel :: JSString -> JSString -> JSString -> JSString
jsRunModel exportFormat modelNameJS modelTextJS =
  let modelName = fromJSString modelNameJS
      exportFormat' = fromJust $ stringToExportFormat $ fromJSString exportFormat
      modelText = fromJSString modelTextJS
      programInput =
        ProgramInput
          { inputModelName = T.pack modelName,
            inputModelFilename = T.pack modelName,
            inputModelText = T.pack modelText,
            -- we don't support parameter creation
            -- in webui yet.
            inputParameterLookup = const Nothing,
            inputExportFormat = exportFormat'
          }
      res = (semanticErrorEffToEither . runWithInput) programInput
      result = case res of
        Left diagErrors -> do
          WasmFailure $ renderString $ layoutPretty defaultLayoutOptions (Diag.prettyDiagnostic Diag.WithUnicode (Diag.TabSize 2) diagErrors)
        Right (diagLints, modelRendered) -> do
          let lintsStr = show (Diag.prettyDiagnostic Diag.WithUnicode (Diag.TabSize 2) diagLints)
          WasmSuccess lintsStr modelRendered
   in (toJSString . BL.unpack . A.encode) result

foreign export javascript "exportFormats"
  jsExportFormats :: JSString

foreign export javascript "runModel"
  jsRunModel :: JSString -> JSString -> JSString -> JSString

#endif
