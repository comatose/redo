module Main where

import Development.Redo
import Development.Redo.Config
import Development.Redo.Future
import Development.Redo.Util

import Control.Applicative
-- import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import SimpleGetOpt
import System.Directory
import System.Environment
import System.Exit
import Text.Read

options :: OptSpec RedoSettings
options = OptSpec {
  progDefaults = RedoSettings {
      help = False,
      inPar = 1,
      files = [],
      shellOpts = " -e",
      debug = False
      },
  progOptions = [Option "h" ["help"] "Display usage."
                 $ NoArg $ \s -> Right s { help = True },

                 Option "v" ["verbose"] "Run with -v (for shells only)."
                 $ NoArg $ \s -> Right s { shellOpts = shellOpts s ++ " -v" },

                 Option "x" ["xtrace"] "Run with -x (for shells only)."
                 $ NoArg $ \s -> Right s { shellOpts = shellOpts s ++ " -x" },

                 Option "p" ["par"] "Set the number of threads to use."
                 $ ReqArg "NUM" $ \a s -> case readMaybe a of
                    Just n | n > 0  -> Right s { inPar = n }
                    _               -> Left "Invalid value for `par`",

                 Option "D" ["debug"] "Display debug information."
                 $ NoArg $ \s -> Right s { debug = True }],

  progParamDocs = [ ("FILES",   "Target list") ],
  progParams = \p s -> Right s { files = files s ++ [p] }}

main :: IO ()
main = do
  settings <- getOpts options
  when (help settings) $ dumpUsage options >> exitSuccess
  bracket_ (initialize settings) finalize $ do
    dir <- getCurrentDirectory
    -- Redo targets are created from the arguments.
    let targets = map (redoTargetFromDir dir) (files settings)
    catch (main' settings targets) $ \e -> case e of
      (NoDoFileExist t) -> die $ "no rule to make " ++ quote t
      (DoExitFailure t d err) -> die $ d ++ " for " ++ t ++ " failed with exitcode " ++ show err
      (CyclicDependency f) -> die $ "cyclic dependency detected for " ++ f
      (TargetNotGenerated t d) -> die $ d ++ " didn't generate " ++ t
      (InvalidDependency f) -> die $ f ++ " has invalid dependency"
      (UnknownRedoCommand cmd) -> die $ "unknown command: " ++ cmd
  where
    die s = printError s >> exitFailure
    main' settings targets = do
      cmd <- getProgName
      case cmd of
        "redo" -> parRedo parallelBuild targets
        "redo-ifchange" -> parRedo parallelBuild targets
        "redo-ifcreate" ->
          -- `redo-ifchange` and `redo-ifcreate` are spawned from another `redo` process
          -- with a file path given via an environment variable.
          -- The file is used to store the dependency information.
          case callerDepsPath of
            (Just depsPath) -> mapM_ (recordDependency depsPath . NonExistingDependency) targets
            Nothing -> return ()
        _ -> throwIO $ UnknownRedoCommand cmd
      when (callDepth == 0) $ printSuccess "done"
    parRedo _ [] = return ()
    parRedo 1 targets = do
      sigs <- withoutProcessorToken $ mapM redo targets
      case callerDepsPath of
        (Just depsPath) -> zipWithM_ (\f -> recordDependency depsPath . ExistingDependency f) targets sigs
        Nothing -> return ()
    parRedo _ targets@(t:ts) = bracket
      -- Targets except the 1st one are handled by sub-threads (using 'async').
      -- Each thread tries to acquire a target lock and a processor token.
      (mapM (async . redo) ts)
      (mapM cancel) $
      -- The main thread owns its own processor token when it starts.
      -- Thus, it release the token before invoking 'redo' and waiting for futures.
      \futures -> withoutProcessorToken $ do
        -- Futures return file signatures of the targets.
        sigs <- liftA2 (:) (redo t) (mapM wait futures)
        case callerDepsPath of
            (Just depsPath) -> zipWithM_ (\f -> recordDependency depsPath . ExistingDependency f) targets sigs
            Nothing -> return ()
