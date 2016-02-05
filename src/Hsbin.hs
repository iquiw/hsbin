{-# LANGUAGE OverloadedStrings #-}
module Hsbin where

import Data.Yaml (decodeFileEither)
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Hsbin.Command
import Hsbin.Types

runHsbin :: HsbinEnv -> HsbinConfig -> [String] -> IO ()
runHsbin henv hcfg args = do
    let (cmd, args') = lookupCommand args
    cmdAction cmd henv hcfg args'

readHsbinConfig :: FilePath -> IO HsbinConfig
readHsbinConfig file = do
    e <- decodeFileEither file
    case e of
        Right cfg -> return cfg
        Left  err -> error $ show err

initHsbin :: HsbinEnv -> IO HsbinConfig
initHsbin henv = do
    createDirectoryIfMissing False $ heAppDir henv
    createDirectoryIfMissing False $ heBinDir henv
    createDirectoryIfMissing False $ heHashDir henv
    let configPath = heConfigPath henv
    b <- doesFileExist configPath
    if b
        then readHsbinConfig configPath
        else return defaultHsbinConfig
