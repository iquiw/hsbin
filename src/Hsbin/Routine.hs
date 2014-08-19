module Hsbin.Routine where

import Control.Monad (unless)
import System.Directory
import System.Exit
import System.FilePath ((</>))
import System.IO
import System.Process

import Hsbin.Types

compile :: HsbinEnv -> HsbinScript -> IO ()
compile henv hscr = do
    let args = hsOpts hscr ++ [ "-outputdir", heTmpDir henv
                              , "-o", heTmpDir henv </> hsName hscr
                              , hsPath hscr]
    ph <- runProcess "ghc" args Nothing Nothing Nothing Nothing Nothing
    ec <- waitForProcess ph
    case ec of
        ExitSuccess -> copyFile
                       (heTmpDir henv </> exe (hsName hscr))
                       (heBinDir henv </> exe (hsName hscr))

        _ -> error ("hsbin: " ++ hsName hscr ++ " compilation failed")

execute :: HsbinEnv -> HsbinScript -> [String] -> IO ()
execute henv hscr args = do
    ph <- runProcess (hsBinPath henv hscr) args
          Nothing Nothing Nothing Nothing Nothing
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $
        error ("hsbin: " ++ hsName hscr ++ " execution failed")

msg :: String -> IO ()
msg = hPutStrLn stderr
