module Main where

import Development.Redo
import Development.Redo.Config
import Development.Redo.Util

import Control.Concurrent
import Control.Exception
import Control.Monad
import SimpleGetOpt
import System.Directory
import System.Environment
import System.Exit
import Text.Read

data RedoSettings = RedoSettings {
  help :: Bool,
  verbose :: Bool,
  xtrace :: Bool,
  inPar   :: Int,
  files   :: [FilePath]
  } deriving (Show, Read)

options :: OptSpec RedoSettings
options = OptSpec {
  progDefaults = RedoSettings {
      help    = False,
      verbose = False,
      xtrace  = False,
      inPar   = 1,
      files   = []
      },
  progOptions = [Option "h" ["help"] "Display usage."
                 $ NoArg $ \s -> Right s { help = True },

                 Option "v" ["verbose"] "Display more information while working."
                 $ NoArg $ \s -> Right s { verbose = True },

                 Option "x" ["xtrace"] "Display more information while working."
                 $ NoArg $ \s -> Right s { xtrace = True },

                 Option "p" ["par"] "The number of parallelism."
                 $ ReqArg "NUM" $ \a s -> case readMaybe a of
                    Just n | n > 0  -> Right s { inPar = n }
                    _               -> Left "Invalid value for `par`"],

  progParamDocs = [ ("FILES",   "The files that need processing.") ],
  progParams = \p s -> Right s { files = files s ++ [p] }}

main :: IO ()
main = do
  targets <- initialize
  catch (main' targets) $ \e -> case e of
    (NoDoFileExist t) -> printError $ "no rule to make " ++ quote t
    (DoExitFailure t err) -> printError $ t ++ " failed with exitcode " ++ show err
    (CyclicDependency f) -> printError $ "cyclic dependency detected for " ++ f
    (TargetNotGenerated f) -> printError $ f ++ " was not generated"
    (InvalidDependency f) -> printError $ f ++ " has invalid dependency"
    (UnknownRedoCommand cmd) -> printError $ "unknown command: " ++ cmd
  finalize
  where
    main' targets = do
      -- `redo-ifchange` and `redo-ifcreate` are spawned from another `redo` process
      -- with a file path given via an environment variable.
      -- The file is used to store the dependency information.
      maybeDepsPath <- lookupEnv envDependencyPath
      cmd <- getProgName
      case cmd of
        "redo" -> parRedo targets
        "redo-ifchange" -> parRedo targets
        "redo-ifcreate" ->
          case maybeDepsPath of
            (Just depsPath) -> mapM_ (addDependency depsPath . NonExistingDependency) targets
            Nothing -> return ()
        _ -> throwIO $ UnknownRedoCommand cmd
      callDepth <- getCallDepth
      when (callDepth == 0) $ printSuccess "done"
    parRedo [] = return ()
    parRedo (t:ts) = do
      sid <- getSemaphoreID
      mapM_ (forkChild . withProcessorToken sid . redo) ts
      redo t
      withoutProcessorToken sid waitForChildren

initialize :: IO [RedoTarget]
initialize = do
  settings <- getOpts options
  callDepth <- getCallDepth
  when (callDepth == 0) $ do {
    createFile printLockPath;
    when (help settings) (dumpUsage options >> exitSuccess);
    setEnv envShellOptions $ unwords [optsToStr settings verbose "-v",
                                     optsToStr settings xtrace "-x"];
    createProcessorTokens (inPar settings - 1);
    setNumCapabilities $ inPar settings;
    }
  dir <- getCurrentDirectory
  -- Redo targets are created from the arguments.
  return $ map (redoTargetFromDir dir) (files settings)
 where optsToStr settings p o = if p settings then o else ""

finalize :: IO ()
finalize = do
  callDepth <- getCallDepth
  when (callDepth == 0) destroyProcessorTokens
