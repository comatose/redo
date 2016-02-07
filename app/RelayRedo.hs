module Main where

import Development.Redo

import Control.Exception
import System.Environment
import System.Exit

main :: IO ()
main = bracket getArgs (const releaseProcessorToken) $
  \[target] -> relayRedo target `catch` \e -> case e of
    (NoDoFileExist t) -> die $ "no rule to make " ++ t
    (DoExitFailure "" "" _) -> exitFailure
    (DoExitFailure t d err) -> die $ d ++ " for " ++ t ++ " failed with exitcode " ++ show err
    (CyclicDependency f) -> die $ "cyclic dependency detected for " ++ f
    (TargetNotGenerated t d) -> die $ d ++ " didn't generate " ++ t
    (InvalidDependency f) -> die $ f ++ " has invalid dependency"
    (UnknownRedoCommand cmd) -> die $ "unknown command: " ++ cmd
 where
    die s = printError s >> exitFailure
