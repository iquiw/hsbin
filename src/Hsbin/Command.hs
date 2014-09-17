module Hsbin.Command where

import Control.Monad (unless)
import Data.Maybe (listToMaybe)

import Hsbin.Hash
import Hsbin.Routine
import Hsbin.Types

type Action = HsbinEnv -> HsbinConfig -> [String] -> IO ()

data Command = Command
    { cmdNames  :: [String]
    , cmdAction :: Action
    , cmdDescr  :: ([String], String)
    }

commands :: [Command]
commands = [ cmdHelp
           , Command ["clean"] actClean
             ([], "clean garbages")
           , Command ["list", "ls"] actList
             ([], "list available scripts")
           , Command ["run"] actRun
             (["NAME"], "run script NAME, build it if necessary")
           , Command ["update"] actUpdate
             (["[-f] [NAME..]"],
              "compile all or specified scripts if necessary")
           ]

cmdHelp :: Command
cmdHelp = Command ["help"] actHelp ([], "show this help")

lookupCommand :: [String] -> (Command, [String])
lookupCommand (cname:args) =
    case listToMaybe $ filter ((cname `elem`) . cmdNames) commands of
        Just cmd -> (cmd, args)
        Nothing  -> (cmdHelp, [])
lookupCommand _            = (cmdHelp, [])

actClean :: Action
actClean henv hcfg _ = do
    tmps <- cleanTmp henv hcfg
    unless (null tmps) $ msgLn $ "Tmp removed  : " ++ unwords tmps

    hashes <- cleanHash henv hcfg
    unless (null hashes) $ msgLn $ "Hash removed : " ++ unwords hashes

    bins <- cleanBin henv hcfg
    unless (null bins) $ msgLn $ "Bin removed  : " ++ unwords bins

actList :: Action
actList _ hcfg _ = do
    let scrs = hcScripts hcfg
    msg $ unlines $ "Available scripts:" : map (("  " ++) . hsName) scrs

actRun :: Action
actRun henv hcfg (name:args) =
    case lookupScript hcfg name of
        Just hscr -> do
            h <- hscrHash hscr
            b <- eqHash henv hscr h
            unless b $ do
                compile henv hscr
                writeHash henv hscr h
            execute henv hscr args
        Nothing   -> msgLn $ "Script not found: " ++ name
actRun _ _ [] = msgLn "hsbin: run needs script name"


actUpdate :: Action
actUpdate henv hcfg args =
    case parseArgs args of
        (force, [])    -> mapM_ (update force) $ hcScripts hcfg
        (force, args') -> mapM_ (\n -> maybe (nfnd n) (update force) $
                                       lookupScript hcfg n) args'
  where
    parseArgs []        = (False, [])
    parseArgs ("-f":xs) = (True, xs)
    parseArgs xs        = (False, xs)

    nfnd n = msgLn $ "[NOT FOUND] " ++ n

    update force hscr = do
        h <- hscrHash hscr
        same <- eqHash henv hscr h
        let (needCompile, tag) = case (same, force) of
                (False, _)    -> (True,  "[COMPILED]  ")
                (True, False) -> (False, "[LATEST]    ")
                (True, True)  -> (True,  "[FORCE]     ")
        if needCompile
            then do compile henv hscr
                    writeHash henv hscr h
                    msgLn $ tag ++ hsName hscr
            else msgLn $ tag ++ hsName hscr

actHelp :: Action
actHelp _ _ _ = help

help :: IO ()
help = msg $ unlines $
       [ "usage: hsbin COMMAND [ARG..]"
       , ""
       , "commands:"
       ] ++ map descr commands
  where
    align n s = take n $ s ++ replicate n ' '

    descr cmd = let (name:aliases) = cmdNames cmd
                    (args, desc)   = cmdDescr cmd
                in align 30 ("  " ++ name ++ " " ++ unwords args)
                   ++ ": " ++ desc
                   ++ if null aliases
                      then ""
                      else " (" ++ unwords aliases ++ ")"
