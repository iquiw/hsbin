module Hsbin.Routine where

import Control.Exception (finally)
import Control.Monad (unless)
import System.Directory (copyFile, createDirectoryIfMissing,
                         removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Process

import Hsbin.Types

compile :: HsbinEnv -> HsbinScript -> IO ()
compile henv hscr = do
    createDirectoryIfMissing True tmpDir
    go `finally` removeDirectoryRecursive tmpDir
  where
    tmpDir = hsTmpDir henv hscr
    go = do
        let args = hsOpts hscr ++ [ "-outputdir", tmpDir
                                  , "-o", hsTmpBinPath henv hscr
                                  , hsPath hscr]
        ph <- runProcess "ghc" args Nothing Nothing Nothing Nothing Nothing
        ec <- waitForProcess ph
        case ec of
            ExitSuccess -> copyFile
                           (hsTmpBinPath henv hscr)
                           (hsBinPath henv hscr)
            _           -> error $ hsName hscr ++ " compilation failed"

execute :: HsbinEnv -> HsbinScript -> [String] -> IO ()
execute henv hscr args = do
    ph <- runProcess (hsBinPath henv hscr) args
          Nothing Nothing Nothing Nothing Nothing
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ error $ hsName hscr ++ " execution failed"

msg :: String -> IO ()
msg = hPutStr stderr

msgLn :: String -> IO ()
msgLn = hPutStrLn stderr
