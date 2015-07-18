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
             (["[-l]"], "list available scripts")
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
    unless (null tmps) $
        msgLn $ align 12 "Tmp removed" ++ " : " ++ unwords tmps

    hashes <- cleanHash henv hcfg
    unless (null hashes) $
        msgLn $ align 12 "Hash removed" ++ " : " ++ unwords hashes

    bins <- cleanBin henv hcfg
    unless (null bins) $
        msgLn $ align 12 "Bin removed" ++ " : " ++ unwords bins

actList :: Action
actList _ hcfg args = do
    let scrs = hcScripts hcfg
        fmt  = if args == ["-l"] then long else short
    msg $ unlines $ "Available scripts:" : map fmt scrs
  where
    short    = ("  " ++) . hsName
    long scr = "  " ++ align 12 (hsName scr) ++ hsPath scr

actRun :: Action
actRun henv hcfg (name:args) =
    case lookupScript hcfg name of
        Just hscr -> do
            h <- hscrHash hscr
            b <- eqHash henv hscr h
            e <- doesBinExist henv hscr
            unless (b && e) $ do
                compile henv hcfg hscr
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

    nfnd n = msgLn $ align 12 "[NOT FOUND]" ++ n

    update force hscr = do
        h <- hscrHash hscr
        same <- eqHash henv hscr h
        e <- doesBinExist henv hscr
        let (needCompile, tag) = case (same && e, force) of
                (False, _)    -> (True,  align 12 "[COMPILED]")
                (True, False) -> (False, align 12 "[LATEST]")
                (True, True)  -> (True,  align 12 "[FORCE]")
        if needCompile
            then do compile henv hcfg hscr
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
    descr cmd = let (name:aliases) = cmdNames cmd
                    (args, desc)   = cmdDescr cmd
                in align 30 ("  " ++ name ++ " " ++ unwords args)
                   ++ ": " ++ desc
                   ++ if null aliases
                      then ""
                      else " (" ++ unwords aliases ++ ")"
