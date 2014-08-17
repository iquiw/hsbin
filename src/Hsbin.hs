{-# LANGUAGE OverloadedStrings #-}
module Hsbin where

import           Control.Monad (unless)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import           System.Directory
import           System.Exit
import           System.FilePath ((</>))
import           System.Process

import           Hsbin.Types

runHsbin :: HsbinEnv -> HsbinConfig -> [String] -> IO ()
runHsbin henv hcfg _ = mapM_ (compile henv) $ hcScripts hcfg

readHsbinConfig :: FilePath -> IO HsbinConfig
readHsbinConfig file = do
    b <- LB.readFile file
    case eitherDecode' b of
        Right cfg -> return cfg
        Left  err -> error err

compile :: HsbinEnv -> HsbinScript -> IO ()
compile henv hs = do
    let args = hsOpts hs ++ [ "-outputdir", heTmpDir henv
                            , "-o", heTmpDir henv </> hsName hs
                            , hsPath hs]
    ph <- runProcess "ghc" args Nothing Nothing Nothing Nothing Nothing
    ec <- waitForProcess ph
    case ec of
        ExitSuccess -> copyFile
                       (heTmpDir henv </> exe (hsName hs))
                       (heBinDir henv </> exe (hsName hs))

        _ -> error ("hsbin: " ++ hsName hs ++ "compilation failed")
