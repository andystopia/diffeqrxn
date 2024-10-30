{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoFieldSelectors #-}

module JSONExporter (fromResolved, toString) where

import Data.Aeson (ToJSON)
import GHC.Generics
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TL

data JSONVariable = JSONVariable
  { name :: String,
    documentation :: String,
    defaultValue :: Maybe String
  }
  deriving (Generic)

instance ToJSON JSONVariable

data JSONExport = JSONExport
  { parameters :: [JSONVariable],
    stateVariables :: [JSONVariable],
    modelName :: String
  }
  deriving (Generic)

instance ToJSON JSONExport

fromResolved :: String -> [String] -> [String] -> JSONExport
fromResolved modelName stateVars parameters =
  JSONExport
    { parameters = (\p -> JSONVariable p "" Nothing) <$> parameters,
      stateVariables = (\p -> JSONVariable p "" Nothing) <$> stateVars,
      modelName = modelName
    }

toString :: JSONExport -> String
toString = TL.unpack . encodeToLazyText
