module Main where

import Development.Redo
import Development.Redo.Config
import Development.Redo.Util

import Control.Exception
import Control.Monad
import SimpleGetOpt
import System.Directory
import System.Environment
import System.Exit

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
  progOptions = [Option "h" ["help"]
                 "Display usage."
                 $ NoArg $ \s -> Right s { help = True }
                ,
                 Option "v" ["verbose"]
                 "Display more information while working."
                 $ NoArg $ \s -> Right s { verbose = True }
                ,
                 Option "x" ["xtrace"]
                 "Display more information while working."
                 $ NoArg $ \s -> Right s { xtrace = True }
                ],
  progParamDocs = [ ("FILES",   "The files that need processing.") ],
  progParams = \p s -> Right s { files = files s ++ [p] }}

initialize :: IO [RedoTarget]
initialize = do
  settings <- getOpts options
  callDepth <- getCallDepth
  when (callDepth == 0) $ do {
    when (help settings) (dumpUsage options >> exitSuccess);
    setEnv envShellOptions $ unwords [optsToStr settings verbose "-v",
                                     optsToStr settings xtrace "-x"];
    }
  dir <- getCurrentDirectory
  -- Redo targets are created from the arguments.
  return $ map (redoTargetFromDir dir) (files settings)
 where optsToStr settings p o = if p settings then o else ""

main :: IO ()
main = (initialize >>= main')
  `catch`
  \e -> case e of
       (NoDoFileExist t) -> die $ "no rule to make " ++ quote t
       (DoExitFailure t err) -> die $ t ++ " failed with exitcode " ++ show err
       (CyclicDependency f) -> die $ "cyclic dependency detected for " ++ f
       (TargetNotGenerated f) -> die $ f ++ " was not generated"
       (InvalidDependency f) -> die $ f ++ " has invalid dependency"
       (UnknownRedoCommand cmd) -> die $ "unknown command: " ++ cmd
  where
    die err = printError err >> exitFailure
    main' targets = do
      -- `redo-ifchange` and `redo-ifcreate` are spawned from another `redo` process
      -- with a file path given via an environment variable.
      -- The file is used to store the dependency information.
      maybeDepsPath <- lookupEnv envDependencyPath
      cmd <- getProgName
      case cmd of
        "redo" -> mapM_ redo targets
        "redo-ifchange" -> do
          -- Signature values of targets will be stored as dependency information.
          sigs <- mapM redo targets
          case maybeDepsPath of
            (Just depsPath) -> do
              let deps = zipWith ExistingDependency targets sigs
              mapM_ (addDependency depsPath) deps
            Nothing -> return ()
        "redo-ifcreate" ->
          case maybeDepsPath of
            (Just depsPath) -> mapM_ (addDependency depsPath . NonExistingDependency) targets
            Nothing -> return ()
        _ -> throwIO $ UnknownRedoCommand cmd
      callDepth <- getCallDepth
      when (callDepth == 0) $ printSuccess "done"
