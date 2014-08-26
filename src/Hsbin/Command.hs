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
commands = [ Command ["help"] actHelp
             ([], "show this help")
           , Command ["list", "ls"] actList
             ([], "list available scripts")
           , Command ["run"] actRun
             (["NAME"], "run script NAME, build it if necessary")
           , Command ["update"] actUpdate
             (["[NAME..]"], "compile all or specified scripts it if necessary")
           ]

cmdHelp :: Command
cmdHelp = head commands

lookupCommand :: [String] -> (Command, [String])
lookupCommand (cname:args) =
    case listToMaybe $ filter ((cname `elem`) . cmdNames) commands of
        Just cmd -> (cmd, args)
        Nothing  -> (cmdHelp, [])
lookupCommand _            = (cmdHelp, [])

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
    case args of
        [] -> mapM_ update $ hcScripts hcfg
        _  -> mapM_ (\n -> maybe (nfnd n) update $ lookupScript hcfg n) args
  where
    nfnd n = msgLn $ "[NOT FOUND] " ++ n

    update hscr = do
        h <- hscrHash hscr
        b <- eqHash henv hscr h
        if b
            then msgLn $ "[LATEST]    " ++ hsName hscr
            else do compile henv hscr
                    writeHash henv hscr h
                    msgLn $  "[COMPILED]  " ++ hsName hscr

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
