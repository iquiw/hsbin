{-# LANGUAGE OverloadedStrings #-}
module Hsbin where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LB

import           Hsbin.Command
import           Hsbin.Types

runHsbin :: HsbinEnv -> HsbinConfig -> [String] -> IO ()
runHsbin henv hcfg args = let (cmd, args') = lookupCommand args
                          in cmdAction cmd henv hcfg args'

readHsbinConfig :: FilePath -> IO HsbinConfig
readHsbinConfig file = do
    b <- LB.readFile file
    case eitherDecode' b of
        Right cfg -> return cfg
        Left  err -> error err
