module Main where

import Control.Applicative ((<$>), (<*>))
import System.Directory (getAppUserDataDirectory, getTemporaryDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Hsbin
import Hsbin.Types

main :: IO ()
main = do
    henv <- HsbinEnv
            <$> getAppUserDataDirectory "hsbin"
            <*> ((</> "hsbin") <$> getTemporaryDirectory)
    hcfg <- initHsbin henv
    args <- getArgs
    runHsbin henv hcfg args
