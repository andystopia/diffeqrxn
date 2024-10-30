{-|

This file provides the essential error handling 
capabilities of the program allowing for a more 
declarative approach to displaying errors to the 
user of the program. 

I believe that good descriptive error messages
are a very important part of making pleasant to use
software, and it displeases me seeing results which 
*clearly* expose which language they are written in, 
because this is clearly just visual noise. (Rust panic's
come to mind -- not that that is a design fault in any 
way) 

So the goal of this is to use the Haskell `effectful` library
to provide an abstraction over the wonderful Haskell `diagnose` library, 
allowing for creating and constructing intutiive and pleasant to 
read error messages for this program.

|-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ErrorProvider where

import Effectful
import Effectful.Dispatch.Dynamic (reinterpret, send)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack, throwError)
import Effectful.State.Static.Local (evalState, execState, gets, modify)
import qualified Error.Diagnose as Diag
import Lexer (Span (..))
import Data.Bifunctor (first)
import Control.Monad (forM_, void)




type instance DispatchOf SemanticErrorEff = Dynamic

type instance DispatchOf SemanticErrorBuilder = Dynamic

data SemanticErrorBuilder :: Effect where
  SemSetErrorCode :: String -> SemanticErrorBuilder m ()
  SemSetErrorMessage :: String -> SemanticErrorBuilder m ()
  SemAddMarker :: Span -> Diag.Marker String -> SemanticErrorBuilder m ()
  SemAddHint :: Diag.Note String -> SemanticErrorBuilder m ()
  SemMakeIntoWarning :: SemanticErrorBuilder m ()
  SemMakeError :: SemanticErrorBuilder m ()

semSetErrorCode :: (SemanticErrorBuilder :> es) => String -> (Eff es) ()
semSetErrorCode x = send (SemSetErrorCode x)

-- | set the error message for this error
semSetErrorMessage :: (SemanticErrorBuilder :> es) => String -> (Eff es) ()
semSetErrorMessage x = send (SemSetErrorMessage x)

-- | add a marker (diagnostic) to this error
semAddMarker :: (SemanticErrorBuilder :> es) => Span -> Diag.Marker String -> (Eff es) ()
semAddMarker sp x = send (SemAddMarker sp x)

semAddHint :: (SemanticErrorBuilder :> es) => Diag.Note String -> (Eff es) ()
semAddHint x = send (SemAddHint x)

semMakeIntoWarning :: (SemanticErrorBuilder :> es) => (Eff es) ()
semMakeIntoWarning = send SemMakeIntoWarning

semMakeError :: (SemanticErrorBuilder :> es) => (Eff es) ()
semMakeError = send SemMakeError

data SemErrorBuild = SemErrorBuild
  { _errCode :: Maybe String,
    _errMessage :: String,
    _errMarkers :: [(Span, Diag.Marker String)],
    _notes :: [Diag.Note String],
    _isWarning :: Bool
  }

defaultSemErrorBuild :: SemErrorBuild
defaultSemErrorBuild = SemErrorBuild Nothing "<an unknown error occurred>" [] [] False


semErrorBuildIntoReport :: String -> SemErrorBuild -> Diag.Report String
semErrorBuildIntoReport filepath (SemErrorBuild code msg markers notes isWarn) = (if isWarn then Diag.Warn else Diag.Err) code msg (first (spanToPosition filepath) <$> markers) notes

spanToPosition :: String -> Span -> Diag.Position
spanToPosition filepath (Span spanLine spanStart spanEnd) = Diag.Position (spanLine, spanStart) (spanLine, spanEnd) filepath

runSemanticErrorBuilder :: Eff (SemanticErrorBuilder : es) a -> (Eff es) SemErrorBuild
runSemanticErrorBuilder = reinterpret (execState defaultSemErrorBuild) $ \_ -> \case
  SemSetErrorCode x -> do
    modify (\c -> c {_errCode = Just x})
    return ()
  SemSetErrorMessage x -> do
    modify (\c -> c {_errMessage = x})
    return ()
  SemAddMarker sp msg -> do
    currentMarkers <- gets _errMarkers
    let withAdditional = currentMarkers ++ [(sp, msg)]
    modify (\c -> c {_errMarkers = withAdditional})
    return ()
  SemAddHint msg -> do
    currentHints <- gets _notes
    let withAdditional = currentHints ++ [msg]
    modify (\c -> c {_notes = withAdditional})
    return ()
  SemMakeIntoWarning -> do
    modify (\c -> c {_isWarning = True})
    return ()
  SemMakeError -> do
    modify (\c -> c {_isWarning = False})
    return ()

data SemanticErrorEffData = SemanticErrorEffData
  { diagnostic :: Diag.Diagnostic String,
    semCurrentFile :: String,
    semErrCount :: Int,
    semCurrentFiles :: [(String, String)]
  }

data SemanticErrorEff :: Effect where
  SemPushError :: Diag.Report String -> SemanticErrorEff m ()
  SemPushDiagnostic :: Diag.Diagnostic String -> SemanticErrorEff m ()
  SemBuildError :: Eff '[SemanticErrorBuilder] () -> SemanticErrorEff m ()
  SemAddFile :: String -> String -> SemanticErrorEff m ()
  SemActivateFile :: String -> SemanticErrorEff m ()
  SemSubscope :: Eff '[SemanticErrorEff, Error (Diag.Diagnostic String)] a -> SemanticErrorEff m (Either (Diag.Diagnostic String) a)
  SemCommitIfErrs :: SemanticErrorEff m (Maybe a)
  SemCommit :: SemanticErrorEff m a

runSemanticErrorEff :: (Error (Diag.Diagnostic String) :> es) => Eff (SemanticErrorEff : es) a -> (Eff es) a
runSemanticErrorEff = do
  let addReport repo = modify (\c -> c {diagnostic = diagnostic c `Diag.addReport` repo, semErrCount = semErrCount c + 1})

  reinterpret (evalState (SemanticErrorEffData mempty "" 0 [])) $ \_ -> \case
    SemPushError err -> do
      void $ addReport err
    SemBuildError err -> do
      currentFile <- gets semCurrentFile
      let err' = semErrorBuildIntoReport currentFile ((runPureEff . runSemanticErrorBuilder) err)
      void $ addReport err'
    SemAddFile file contents -> do
      void $ modify (\c -> c {diagnostic = Diag.addFile (diagnostic c) file contents, semCurrentFiles = semCurrentFiles c ++ [(file, contents)]})
    SemActivateFile file -> do
      void $ modify (\c -> c {semCurrentFile = file})
    SemCommit -> do
      current <- gets diagnostic
      throwError current
    SemSubscope eff -> do
      files <- gets semCurrentFiles
      curFile <- gets semCurrentFile
      let res = runPureEff . runErrorNoCallStack @(Diag.Diagnostic String) . runSemanticErrorEff $ do
            forM_ files $ \(filepath, filename) -> do
              semAddFile filepath filename
            semActivateFile curFile
            eff
      return res
    SemPushDiagnostic diag -> do
      forM_ (Diag.reportsOf diag) $ \repo -> do
        void $ addReport repo
    SemCommitIfErrs -> do
      current <- gets diagnostic
      nErrs <- gets semErrCount

      if nErrs /= 0
        then throwError current
        else pure Nothing

-- throwError current

semSubscope :: (SemanticErrorEff :> es) => Eff '[SemanticErrorEff, Error (Diag.Diagnostic String)] a -> (Eff es) (Either (Diag.Diagnostic String) a)
semSubscope = send . SemSubscope

semLiftMaybe :: (SemanticErrorEff :> es) => Maybe a -> Eff '[SemanticErrorBuilder] () -> Eff es a
semLiftMaybe (Just a) _ = pure a
semLiftMaybe Nothing eff = do
  semBuildError eff
  semCommit

effEither :: (Error e :> es) => Either e a -> Eff es a
effEither (Left diag) = throwError diag
effEither (Right a) = return a

semPushDiagnostic :: (SemanticErrorEff :> es) => Diag.Diagnostic String -> (Eff es) ()
semPushDiagnostic e = send (SemPushDiagnostic e)

semLiftEitherDiag :: (SemanticErrorEff :> es) => Either (Diag.Diagnostic String) k -> (Eff es) k
semLiftEitherDiag (Right k) = return k
semLiftEitherDiag (Left e) = do
  semPushDiagnostic e
  semCommit

semPushError :: (SemanticErrorEff :> es) => Diag.Report String -> (Eff es) ()
semPushError e = send (SemPushError e)

semCommit :: (SemanticErrorEff :> es) => (Eff es) a
semCommit = send SemCommit

semAddFile :: (SemanticErrorEff :> es) => String -> String -> Eff es ()
semAddFile name contents = send (SemAddFile name contents)

semActivateFile :: (SemanticErrorEff :> es) => String -> Eff es ()
semActivateFile name = send (SemActivateFile name)

semCommitIfErrs :: (SemanticErrorEff :> es) => (Eff es) ()
semCommitIfErrs = void (send SemCommitIfErrs)

semBuildError :: (SemanticErrorEff :> es) => Eff '[SemanticErrorBuilder] () -> (Eff es) ()
semBuildError e = send (SemBuildError e)

