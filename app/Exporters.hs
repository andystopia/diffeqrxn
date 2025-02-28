{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Exporters (juliaExporter, asciiExporter, typstExporter) where
import AST
import Parser
import qualified Data.Map as M
import Effectful ((:>), Eff)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified CodeWriter as CW
import qualified Data.Text as T
import Control.Monad (forM_, unless)


typstLit :: String -> String
typstLit lit = concat $ intercalateMany cycler divided
  where
    typstLitH :: String -> String
    typstLitH [v] = [v]
    typstLitH x = "\"" <> x <> "\""

    divided :: [String]
    divided = typstLitH <$> splitOn "_" lit
    cycler :: [String]
    cycler = ["_", "\\_"]

    intercalateMany :: [a] -> [a] -> [a]
    intercalateMany _ [] = []
    intercalateMany _ [a] = [a]
    intercalateMany (current : future) (a : rest) =
      [a, current] ++ intercalateMany (future ++ [current]) rest
    intercalateMany [] _ = error "empty intercalating list passed"

mapSeparateFirst :: (a -> b) -> (a -> b) -> [a] -> [b]
mapSeparateFirst _ _ [] = []
mapSeparateFirst inj _ [v] = [inj v]
mapSeparateFirst inj mapper (a : rest) = inj a : (mapper <$> rest)

exportASTToTypst :: ExportAST -> String
exportASTToTypst (ExportParameter (Parameter text)) = intercalate "_" (typstLit <$> splitOn "_" text)
exportASTToTypst (ExportStateVar lit) = typstLit (joinPolymer ":" lit)
exportASTToTypst export@(ExportProd p) = intercalate " dot.c " $ func' <$> p
  where
    func' = exportASTGroupMaybeASCII exportASTToTypst export
exportASTToTypst x = exportUsingASCII exportASTToTypst x

intercalateStringPeriodically :: Int -> String -> [String] -> [String]
intercalateStringPeriodically maxLen inner list =
  intercalateStringPeriodicallyH 0 maxLen inner list
  where
    intercalateStringPeriodicallyH :: Int -> Int -> String -> [String] -> [String]
    intercalateStringPeriodicallyH _ _ _ [] = []
    intercalateStringPeriodicallyH _ _ _ [a] = [a]
    intercalateStringPeriodicallyH count maxLen' inner' (current : rest) =
      if count + length current > maxLen
        then [inner, current] ++ intercalateStringPeriodicallyH (length inner) maxLen' inner' rest
        else current : intercalateStringPeriodicallyH (length current + count) maxLen' inner' rest

exportASTToTypst' :: ExportAST -> String
exportASTToTypst' export = case export of
  (ExportSum s) ->
    concat $
      intercalateStringPeriodically 120 " \\ & " $
        intersperseCondEither
          func'
          ( \case
              ExportNeg v -> Left v
              c -> Right c
          )
          " - "
          " + "
          s
  _ -> exportASTToTypst export
  where
    func' = exportASTGroupMaybeASCII exportASTToTypst export

intersperseCond :: (a -> Bool) -> a -> a -> [a] -> [a]
intersperseCond _ _ _ [] = []
intersperseCond _ _ _ [v] = [v]
intersperseCond cond l r (a : b : rest) = [a, if cond b then l else r] ++ intersperseCond cond l r (b : rest)

intersperseCondT :: (a -> b) -> (a -> Bool) -> b -> b -> [a] -> [b]
intersperseCondT _ _ _ _ [] = []
intersperseCondT conv _ _ _ [v] = [conv v]
intersperseCondT conv cond l r (a : b : rest) =
  [conv a, if cond b then l else r] ++ intersperseCondT conv cond l r (b : rest)

intersperseCondEither :: (a -> b) -> (a -> Either a a) -> b -> b -> [a] -> [b]
intersperseCondEither _ _ _ _ [] = []
intersperseCondEither conv _ _ _ [v] = [conv v]
intersperseCondEither conv cond l r (a : b : rest) =
  [ conv a,
    case cond b of
      Left _ -> l
      Right _ -> r
  ]
    ++ intersperseCondEither conv cond l r ((fromEither . cond) b : rest)

fromEither :: Either a a -> a
fromEither (Left v) = v
fromEither (Right v) = v

joinPolymer :: String -> StateVariable -> String
joinPolymer _ (Monomer i) = i
joinPolymer x (Polymer polys) = intercalate x polys

exportUsingASCII :: (ExportAST -> String) -> ExportAST -> String
exportUsingASCII func export =
  case export of
    (ExportStateVar ident) -> joinPolymer "__di__" ident
    (ExportParameter (Parameter param)) -> param
    (ExportInt int) -> show int
    (ExportNeg str) -> "-" <> func' str
    (ExportSum s) ->
      concat $
        intersperseCondEither
          func'
          ( \case
              ExportNeg v -> Left v
              c -> Right c
          )
          " - "
          " + "
          s
    (ExportProd p) -> intercalate " * " $ func' <$> p
    (ExportPow a b) -> func' a <> "^" <> func' b
    (ExportDiv a b) -> func' a <> "/" <> func' b
  where
    -- we do this to add parantheses so that the expansion
    -- doesn't output something algebraically invalid, by leaving off
    -- parentheses.
    func' = exportASTGroupMaybeASCII func export

exportToASCII :: ExportAST -> String
exportToASCII = exportUsingASCII exportToASCII

-- | Extract the unique chemical names out of a set of equations.
-- (Note: does not currently include )
allChemicalNames :: [Equation] -> [StateVariable]
allChemicalNames eqns = S.toList $ foldMap chemicalNames eqns


typstExporter :: (CW.CodeWriter :> es) => String -> ResolvedVariableModel -> (Eff es) ()
typstExporter _ (ResolvedVariableModel _stateVars _sysParams _ _ _dependent eqns) = do
  CW.scoped "$" $ do
    forM_ (M.toList eqns) $ \(species, definition) -> do
      CW.push $ "(dif " <> joinPolymer ":" species <> ")/(dif t) &= " <> (exportASTToTypst' . simpleExportAST) definition <> "\\"

  CW.push "$"
  return ()

asciiExporter :: (CW.CodeWriter :> es) => String -> ResolvedVariableModel -> (Eff es) ()
asciiExporter _ (ResolvedVariableModel _stateVars _sysParams _ _ _dependent eqns) = do
  forM_ (M.toList eqns) $ \(species, definition) -> do
    CW.push $ joinPolymer "__di__" species <> " = " <> (exportToASCII . simpleExportAST) definition

juliaExporter :: (CW.CodeWriter :> es) => String -> ResolvedVariableModel -> (T.Text -> Maybe T.Text) -> (Eff es) ()
juliaExporter modelName (ResolvedVariableModel stateVars sysParams computed implicit _dependent eqns) defaultValueLookup = do
  let stateVarList = S.toList stateVars
  let sysParamsList = S.toList ((sysParams `S.union` implicit) S.\\ computed)
  let computedParamsList = S.toList computed

  let paramStructName = modelName <> "Params"
  let computedParamStructName = modelName <> "ComputedParams"
  let stateStructName = modelName <> "StateVars"


  CW.scoped ("@Base.kwdef struct " <> paramStructName) $ do
    forM_ sysParamsList $ \var -> do
      let defaultValue = defaultValueLookup ((T.pack . parameterToString) var)
      case defaultValue of
        Just def_value -> CW.push $ parameterToString var <> " = " <> T.unpack def_value
        Nothing -> CW.push $ parameterToString var

  CW.push "end"

  CW.newline

  unless (null computed) $ do
    CW.scoped ("@Base.kwdef struct " <> computedParamStructName) $ do
      forM_ computedParamsList $ \var ->
        CW.push $ parameterToString var

  CW.push "end"
  CW.newline

  CW.scoped ("function Base.convert(::Type{Vector}, s::" <> paramStructName <> ")") $ do
    CW.push $ "return " <> "[" ++ intercalate ", " (("s." <>) . parameterToString <$> sysParamsList) ++ "]"

  CW.push "end"

  CW.newline

  CW.scoped ("function Base.convert(::Type{" <> paramStructName <> "}, fromVec::Vector)") $ do
    CW.push $ "@assert length(fromVec) == " <> show (length sysParamsList)
    CW.push $ "return " <> paramStructName <> "("

    CW.indented $ do
      forM_ (zip sysParamsList [1 :: Int ..]) $ \(var, idx) -> do
        CW.push $ parameterToString var <> " = " <> "fromVec[" <> show idx <> "],"

    CW.push ")"

  CW.push "end"

  CW.newline

  CW.scoped ("@Base.kwdef struct " <> stateStructName) $ do
    forM_ stateVarList $ \var -> do
      CW.push  $ stateVarToJulia var

  CW.push "end"

  CW.newline

  CW.scoped ("function Base.convert(::Type{Vector}, s::" <> stateStructName <> ")") $ do
    CW.push $ "return " <> "[" ++ intercalate ", " (("s." <>) . stateVarToJulia <$> stateVarList) ++ "]"

  CW.push "end"

  CW.newline

  CW.scoped ("function Base.convert(::Type{" <> stateStructName <> "}, fromVec::Vector)") $ do
    CW.push $ "@assert length(fromVec) == " <> show (length stateVars)
    CW.push $ "return " <> stateStructName <> "("

    CW.indented $ do
      forM_ (zip stateVarList [1 :: Int ..]) $ \(var, idx) -> do
        CW.push $ stateVarToJulia var <> " = " <> "fromVec[" <> show idx <> "],"

    CW.push ")"

  CW.push "end"

  CW.newline

  -- juliaAllocatingDiffEq eqns stateVarList computed modelName paramStructName stateStructName
  juliaNonAllocatingDiffEq eqns stateVarList computed modelName paramStructName

  -- let astParameterToJulia' var = parameterToJulia (if var `S.member` computed then "c." else "p.") var
  -- let prefixStateVar var =  "s." <> stateVarToJulia var

  -- -- I believe in Julia this is a valid definition for a differential equation
  -- -- because the type annotations will coerce the types into what you want them to be.
  -- CW.scoped ("function d" <> modelName <> "(sv, " <> "p::" <> paramStructName <> ", t)::Vector") $ do
  --   CW.push ("s::" <> stateStructName <> " = " <> "sv")
  --   forM_ stateVarList $ \stateVar -> do
  --     CW.push ("d" <> stateVarToJulia stateVar <> " = " <> (exportASTToJulia astParameterToJulia' prefixStateVar . simpleExportAST $ (eqns M.! stateVar)))

  --   CW.push $ "return " <> "[" ++ intercalate ", " (("d" <>) . stateVarToJulia <$> stateVarList) ++ "]"

  -- CW.push "end"

  return ()
  where
    stateVarToJulia :: StateVariable -> String
    stateVarToJulia = joinPolymer "__di__"

    -- parameterToJulia :: String -> Parameter -> String
    -- parameterToJulia prefix (Parameter param)= prefix <> param

    parameterToString :: Parameter -> String
    parameterToString (Parameter p) = p

juliaAllocatingDiffEq ::(CW.CodeWriter :> es) => M.Map StateVariable ExportAST -> [StateVariable] -> S.Set Parameter -> String -> String -> String -> (Eff es) () 
juliaAllocatingDiffEq eqns stateVarList computed modelName paramStructName stateStructName = do
  let astParameterToJulia' var = parameterToJulia (if var `S.member` computed then "c." else "p.") var
  let prefixStateVar var =  "s." <> stateVarToJulia var

  -- I believe in Julia this is a valid definition for a differential equation
  -- because the type annotations will coerce the types into what you want them to be.
  CW.scoped ("function d" <> modelName <> "(sv, " <> "p::" <> paramStructName <> ", t)::Vector") $ do
    CW.push ("s::" <> stateStructName <> " = " <> "sv")
    forM_ stateVarList $ \stateVar -> do
      CW.push ("d" <> stateVarToJulia stateVar <> " = " <> (exportASTToJulia astParameterToJulia' prefixStateVar . simpleExportAST $ (eqns M.! stateVar)))

    CW.push $ "return " <> "[" ++ intercalate ", " (("d" <>) . stateVarToJulia <$> stateVarList) ++ "]"

  CW.push "end"

  return ()
  where
    stateVarToJulia :: StateVariable -> String
    stateVarToJulia = joinPolymer "__di__"

    parameterToJulia :: String -> Parameter -> String
    parameterToJulia prefix (Parameter param)= prefix <> param

-- | the idea here is that julia really 
-- | prefers that you don't allocate when you're writing
-- | your evaluation function, so this one tries to implement that
juliaNonAllocatingDiffEq ::(CW.CodeWriter :> es) => M.Map StateVariable ExportAST -> [StateVariable] -> S.Set Parameter -> String -> String  -> (Eff es) () 
juliaNonAllocatingDiffEq eqns stateVarList computed modelName paramStructName  = do
  let astParameterToJulia' var = parameterToJulia (if var `S.member` computed then "c." else "p.") var
  let stateVarMap = M.fromList (zip stateVarList [1 :: Int ..])
  let prefixStateVar var = "sv[" <> show (stateVarMap M.! var) <> "]" 

  -- I believe in Julia this is a valid definition for a differential equation
  -- because the type annotations will coerce the types into what you want them to be.
  CW.scoped ("function d" <> modelName <> "(du, sv, " <> "p::" <> paramStructName <> ", t)") $ do
    forM_ stateVarList $ \stateVar -> do
      CW.push ("du[" <> show (stateVarMap M.! stateVar) <> "] = " <> (exportASTToJulia astParameterToJulia' prefixStateVar . simpleExportAST $ (eqns M.! stateVar)))

    CW.push "return nothing"

  CW.push "end"

  return ()
  where
    parameterToJulia :: String -> Parameter -> String
    parameterToJulia prefix (Parameter param)= prefix <> param

exportASTToJulia :: (Parameter -> String) -> (StateVariable -> String) -> ExportAST -> String
exportASTToJulia _ stateVarToString (ExportStateVar stateVar) = stateVarToString stateVar
exportASTToJulia paramToString _ (ExportParameter parameter) = paramToString parameter
exportASTToJulia paramToString stateVarToString x = exportUsingASCII (exportASTToJulia paramToString stateVarToString) x

