module Main where

import Development.Redo
import Development.Redo.Config
import Development.Redo.Future
import Development.Redo.Util

import Control.Applicative
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

                 Option "v" ["verbose"] "Display more information while working."
                 $ NoArg $ \s -> Right s { shellOpts = shellOpts s ++ " -v" },

                 Option "x" ["xtrace"] "Display more information while working."
                 $ NoArg $ \s -> Right s { shellOpts = shellOpts s ++ " -x" },

                 Option "p" ["par"] "The number of parallelism."
                 $ ReqArg "NUM" $ \a s -> case readMaybe a of
                    Just n | n > 0  -> Right s { inPar = n }
                    _               -> Left "Invalid value for `par`",

                 Option "D" ["debug"] "Display debug information."
                 $ NoArg $ \s -> Right s { debug = True }],

  progParamDocs = [ ("FILES",   "The files that need processing.") ],
  progParams = \p s -> Right s { files = files s ++ [p] }}

main :: IO ()
main = do
  settings <- getOpts options
  when (help settings) $ dumpUsage options >> exitSuccess
  bracket_ (initialize settings) finalize $ do
    dir <- getCurrentDirectory
    -- Redo targets are created from the arguments.
    let targets = map (redoTargetFromDir dir) (files settings)
    catch (main' targets) $ \e -> case e of
      (NoDoFileExist t) -> die $ "no rule to make " ++ quote t
      (DoExitFailure t d err) -> die $ d ++ " for " ++ t ++ " failed with exitcode " ++ show err
      (CyclicDependency f) -> die $ "cyclic dependency detected for " ++ f
      (TargetNotGenerated t d) -> die $ d ++ " didn't generate " ++ t
      (InvalidDependency f) -> die $ f ++ " has invalid dependency"
      (UnknownRedoCommand cmd) -> die $ "unknown command: " ++ cmd
  where
    die err = printError err >> exitFailure
    main' targets = do
      cmd <- getProgName
      case cmd of
        "redo" -> parRedo targets
        "redo-ifchange" -> parRedo targets
        "redo-ifcreate" ->
          -- `redo-ifchange` and `redo-ifcreate` are spawned from another `redo` process
          -- with a file path given via an environment variable.
          -- The file is used to store the dependency information.
          case callerDepsPath of
            (Just depsPath) -> mapM_ (addDependency depsPath . NonExistingDependency) targets
            Nothing -> return ()
        _ -> throwIO $ UnknownRedoCommand cmd
      when (callDepth == 0) $ printSuccess "done"
    parRedo [] = return ()
    parRedo targets@(t:ts) = bracket
      -- targets except the 1st one are handled by child threads.
      -- redo first acquires a target lock and then a processor token.
      (mapM (async . redo) ts)
      (mapM cancel) $
      -- 1stt target is handled by the main thread,
      -- which already has a processor token from the beginning of the execution.
      -- thus, release the token before redo.
      \futures -> withoutProcessorToken $ do
        sigs <- liftA2 (:) (redo t) (mapM wait futures)
        case callerDepsPath of
            (Just depsPath) -> zipWithM_ (\s -> addDependency depsPath . ExistingDependency s) targets sigs
            Nothing -> return ()
