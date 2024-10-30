{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TomlParamProvider (getDefaultVariableLUT) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful (Eff, Effect, IOE, (:>))
import ErrorProvider
import GHC.Generics
import Toml
import Utils (joinWithAnd)
import Prelude hiding (cycle)

-- newtype Entries = Entries (MS.Map T.Text T.Text) deriving (Show, Generic);
data System = System
  { name :: T.Text,
    inherits :: Maybe T.Text,
    parameters :: MS.Map T.Text T.Text
  }
  deriving (Show, Generic)

newtype SystemVariables = SystemVariables
  { system :: [System]
  }
  deriving (Show, Generic)

newtype ParameterLookup = ParameterLookup (MS.Map T.Text T.Text) deriving (Show)

data LookupParameterSetError
  = DuplicatedSystemName
      { duplicated_name :: T.Text
      }
  | MissingName
      { missing_name :: T.Text,
        inheritance_tree :: [T.Text]
      }
  | CyclicLoop
      { cycle :: [T.Text]
      }
  | MultipleDefinitions
      { collision_map :: MS.Map T.Text [T.Text]
      }
  deriving (Show)

-- | create a mapping from a list of parameter sets to a
-- | Map from parameter name -> list of parameter sets
-- | this could be used to detect if a parameter is repeated
systemParametersToSystemName :: [System] -> MS.Map T.Text [T.Text]
systemParametersToSystemName =
  foldr
    ( \p ->
        MS.unionWith
          (<>)
          (MS.fromList $ (,[name p]) <$> MS.keys (parameters p))
    )
    MS.empty

lookupParameterSet :: SystemVariables -> T.Text -> Either LookupParameterSetError ParameterLookup
-- TODO: multiple definition names should probably be preserved
-- and flagged, but eh, it is what it is.
lookupParameterSet sets set_name = do
  -- gather the inheritance hierarchy
  parents <- grabParents [] (Just set_name)
  -- now transpose the tree to get a parameter name to set name
  -- this lets us detect multiple parameter definitions, _and_ where they happen.
  let parameterDefinitionLocations = systemParametersToSystemName parents
  -- now we want to keep only the ones with multiple definitions (definition count > 1)
  let filteredParameterDefinitionLocations = MS.filter (\x -> length x > 1) parameterDefinitionLocations

  -- if we have any instances of multiple definitions, then return
  -- a multiple definitions error
  if (not . null) filteredParameterDefinitionLocations
    then Left (MultipleDefinitions filteredParameterDefinitionLocations)
    else do
      -- otherwise fold up all into one big set,
      -- and we're all done, we have validated and established
      -- that we have a proper set with few surprises as possible
      return (ParameterLookup (foldMap parameters parents))
  where
    grabParents :: [T.Text] -> Maybe T.Text -> Either LookupParameterSetError [System]
    grabParents _ Nothing = Right []
    grabParents parents (Just inherit)
      | inherit `elem` parents = Left (CyclicLoop (parents ++ [inherit]))
      | otherwise = do
          future <- case filter (\x -> name x == inherit) (system sets) of
            [] -> Left (MissingName set_name (name <$> system sets))
            [x] -> Right x
            (rest : _) -> Left (DuplicatedSystemName (name rest))

          pars <- grabParents (parents ++ [inherit]) (inherits future)
          return (future : pars)

paramsCodec :: TomlCodec System
paramsCodec =
  System
    <$> Toml.diwrap (Toml.text "name") .= name
    <*> Toml.dioptional (Toml.text "inherits") .= inherits
    <*> Toml.tableMap Toml._KeyText Toml.text "parameters" .= parameters

instance HasItemCodec System where
  hasItemCodec = Right paramsCodec

instance HasCodec SystemVariables where
  hasCodec = Toml.table genericCodec

parameterSetsCodec :: TomlCodec SystemVariables
parameterSetsCodec = genericCodec

getDefaultVariableLUT :: (SemanticErrorEff :> es) =>  T.Text -> T.Text -> (Eff es) (T.Text -> Maybe T.Text)
getDefaultVariableLUT modelName modelText = do
  let contents = Toml.decode parameterSetsCodec modelText

  case contents of
    Left failure -> do
      forM_ failure $ \tomlDecodeFail-> do
        semBuildError $ do
          semSetErrorCode "TOML_DE_ERROR"
          semSetErrorMessage $ show tomlDecodeFail
      semCommit
    Right success -> do
      case lookupParameterSet success modelName of
        Left failure ->
          case failure of
            DuplicatedSystemName {duplicated_name = dn} -> do
              semBuildError $ do
                semSetErrorCode "DUPLICATED_SYSTEM_NAME"
                semSetErrorMessage $ T.unpack $ "The system name `" <> dn <> "` occurs twice in the system file"
              semCommit
            MissingName {missing_name = mn, inheritance_tree = it} -> do
              semBuildError $ do
                semSetErrorCode "MISSING_SYSTEM_NAME"
                if null it
                  then semSetErrorMessage $ T.unpack $ "MISSING name `" <> mn <> "."
                  else semSetErrorMessage $ T.unpack $ "Broken inheritance tree. Can't find `" <> mn <> "`. It's inheritance tree goes: " <> T.intercalate " -> " it
              semCommit
            CyclicLoop {cycle = cyc} -> do
              semBuildError $ do
                semSetErrorCode "INHERITANCE_CYCLE"
                semSetErrorMessage $ T.unpack $ "Inheritance cycle encountered: " <> T.intercalate " -> " cyc
              semCommit
            MultipleDefinitions {collision_map = cm} -> do
              forM_ (MS.assocs cm) $ \(var, deflocs) -> do
                semBuildError $ do
                  semSetErrorCode "MULTIPLE_VARIABLE_REDEFINITIONS"
                  semSetErrorMessage $ "Muliple definitions of `" <> T.unpack var <> "`.  It is defined in " <> joinWithAnd (T.unpack . (\x -> "`" <> x <> "``") <$> deflocs)
              semCommit
        Right (ParameterLookup innerMap) -> do
          return (`MS.lookup` innerMap)

