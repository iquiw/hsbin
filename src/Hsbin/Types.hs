{-# LANGUAGE OverloadedStrings #-}
module Hsbin.Types where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Maybe (listToMaybe)
import System.FilePath ((<.>), (</>))
import System.Info (os)

data HsbinEnv = HsbinEnv
    { heAppDir :: FilePath
    , heTmpDir :: FilePath
    } deriving (Eq, Show)

data HsbinConfig = HsbinConfig
    { hcScripts :: [HsbinScript]
    } deriving (Eq, Show)

data HsbinScript = HsbinScript
    { hsName :: String
    , hsPath :: FilePath
    , hsOpts :: [String]
    } deriving (Eq, Show)

instance FromJSON HsbinConfig where
    parseJSON (Object o) = HsbinConfig <$> (o .: "scripts")
    parseJSON _          = fail "HsbinConfig: Object expected"

instance ToJSON HsbinConfig where
    toJSON (HsbinConfig s) = object [ "scripts" .= s ]

instance FromJSON HsbinScript where
    parseJSON (Object o) = HsbinScript
                           <$> (o .: "name")
                           <*> (o .: "path")
                           <*> (o .: "opts")
    parseJSON _          = fail "HsbinScript: Object expected"

instance ToJSON HsbinScript where
    toJSON (HsbinScript n p o) = object [ "name" .= n
                                        , "path" .= p
                                        , "opts" .= o
                                        ]

exe :: FilePath -> FilePath
exe = if os == "mingw32" then (<.> "exe") else id

heConfigPath :: HsbinEnv -> FilePath
heConfigPath =  (</> "config.json") . heAppDir

heBinDir :: HsbinEnv -> FilePath
heBinDir = (</> "bin") . heAppDir

heHashDir :: HsbinEnv -> FilePath
heHashDir = (</> "hash") . heAppDir

hsBinPath :: HsbinEnv -> HsbinScript -> FilePath
hsBinPath henv hscr = heBinDir henv </> exe (hsName hscr)

hsHashPath :: HsbinEnv -> HsbinScript -> FilePath
hsHashPath henv hscr = heHashDir henv </> hsName hscr

hsTmpDir :: HsbinEnv -> HsbinScript -> FilePath
hsTmpDir henv hscr = heTmpDir henv </> hsName hscr

hsTmpBinPath :: HsbinEnv -> HsbinScript -> FilePath
hsTmpBinPath henv hscr = heTmpDir henv </> hsName hscr </> exe (hsName hscr)

lookupScript :: HsbinConfig -> String -> Maybe HsbinScript
lookupScript hcfg name =
    listToMaybe $ filter ((name ==) . hsName) $ hcScripts hcfg
