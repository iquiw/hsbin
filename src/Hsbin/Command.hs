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
    , cmdDescr  :: String
    }

commands :: [Command]
commands = [ Command ["help"] actHelp "show this help"
           , Command ["run"] actRun "run script NAME, build it if necessary"
           ]

cmdHelp :: Command
cmdHelp = head commands

lookupCommand :: [String] -> (Command, [String])
lookupCommand (cname:args) =
    case listToMaybe $ filter ((cname `elem`) . cmdNames) commands of
        Just cmd -> (cmd, args)
        Nothing  -> (cmdHelp, [])
lookupCommand _            = (cmdHelp, [])

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
        Nothing   -> msg $ "Script not found: " ++ name
actRun _ _ [] = msg "hsbin: run needs script name"

actHelp :: Action
actHelp _ _ _ = help

help :: IO ()
help = msg $ unlines
       [ "usage: hsbin COMMAND [ARG..]"
       , ""
       , "commands:"
       , "   help       : show this help."
       , "   run NAME   : run script NAME, build it if necessary."
       ]
