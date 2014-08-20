module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (finally)
import System.Directory ( createDirectoryIfMissing,
                          getAppUserDataDirectory,
                          getTemporaryDirectory, removeDirectoryRecursive )
import System.Environment (getArgs)
import System.FilePath ((</>))

import Hsbin
import Hsbin.Types

main :: IO ()
main = do
    henv <- HsbinEnv
            <$> getAppUserDataDirectory "hsbin"
            <*> ((</> "hsbin") <$> getTemporaryDirectory)
    hcfg <- readHsbinConfig $ heConfigPath henv
    args <- getArgs
    let tmpDir = heTmpDir henv
    createDirectoryIfMissing False $ heAppDir henv
    createDirectoryIfMissing False $ heBinDir henv
    createDirectoryIfMissing False $ heHashDir henv
    createDirectoryIfMissing False tmpDir
    runHsbin henv hcfg args `finally` removeDirectoryRecursive tmpDir
