module Main where

import Development.Redo

import Control.Exception
import Control.Monad
import SimpleGetOpt
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

                 Option "j" ["jobs"] "Set the number of threads to use."
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
  withRedo settings . catch (main' $ files settings) $
    \e -> case e of
      (NoDoFileExist t) -> die $ "no rule to make " ++ t
      (DoExitFailure "" "" _) -> exitFailure
      (DoExitFailure t d err) -> die $ d ++ " for " ++ t ++ " failed with exitcode " ++ show err
      (CyclicDependency f) -> die $ "cyclic dependency detected for " ++ f
      (TargetNotGenerated t d) -> die $ d ++ " didn't generate " ++ t
      (InvalidDependency f) -> die $ f ++ " has invalid dependency"
      (UnknownRedoCommand cmd) -> die $ "unknown command: " ++ cmd
      (TargetAlreadyExist t) -> die $ t ++ " already exists, but not generated by redo"
  where
    die s = printError s >> exitFailure
    main' targets = do
      cmd <- getProgName
      case cmd of
        "redo" -> redo targets
        "redo-ifchange" -> redoIfChange targets
        "redo-ifcreate" -> redoIfCreate targets
        "redo-iftouch" -> redoIfTouch targets
        "redo-status" -> redoStatus targets
        _ -> throwIO $ UnknownRedoCommand cmd
      when (callDepth == 0) $ printSuccess "done"
