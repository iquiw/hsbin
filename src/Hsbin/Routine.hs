module Hsbin.Routine
    ( align
    , cleanBin
    , cleanHash
    , cleanTmp
    , doesBinExist
    , compile
    , execute
    , msg
    , msgLn
    ) where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Monad (filterM, unless)
import System.Directory (copyFile, createDirectoryIfMissing,
                         doesDirectoryExist, doesFileExist,
                         getDirectoryContents,
                         removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
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

doesBinExist :: HsbinEnv -> HsbinScript -> IO Bool
doesBinExist henv hscr = doesFileExist $ hsBinPath henv hscr

cleanBin :: HsbinEnv -> HsbinConfig -> IO [FilePath]
cleanBin henv hcfg = do
    fs <- filterFile (map (exe . hsName) $ hcScripts hcfg) (heBinDir henv)
    mapM_ removeFile fs
    return $ map takeFileName fs

cleanHash :: HsbinEnv -> HsbinConfig -> IO [FilePath]
cleanHash henv hcfg = do
    fs <- filterFile (map hsName $ hcScripts hcfg) (heHashDir henv)
    mapM_ removeFile fs
    return $ map takeFileName fs

cleanTmp :: HsbinEnv -> HsbinConfig -> IO [FilePath]
cleanTmp henv hcfg = do
    let tmpDir = heTmpDir henv
    b <- doesDirectoryExist tmpDir
    if b
        then do
           fs <- filterDir (map hsName $ hcScripts hcfg) tmpDir
           mapM_ removeDirectoryRecursive fs
           return $ map takeFileName fs
        else return []

filterFile :: [String] -> FilePath -> IO [FilePath]
filterFile names dir = filterM doesFileExist =<< filterContents names dir

filterDir :: [String] -> FilePath -> IO [FilePath]
filterDir names dir = filterM doesDirectoryExist =<< filterContents names dir

filterContents :: [String] -> FilePath -> IO [FilePath]
filterContents names dir = map (dir </>)
                           <$> filter (`notElem` ([".", ".."] ++ names))
                           <$> getDirectoryContents dir

msg :: String -> IO ()
msg = hPutStr stderr

msgLn :: String -> IO ()
msgLn = hPutStrLn stderr

align :: Int -> String -> String
align n = take n . (++ replicate n ' ')
