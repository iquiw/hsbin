module Hsbin.Hash where

import           Control.Applicative
import           Crypto.Hash.SHA1
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           System.Directory

import           Hsbin.Types

type Hash = ByteString

eqHash :: HsbinEnv -> HsbinScript -> Hash -> IO Bool
eqHash henv hscr newH = do
    let path = hsHashPath henv hscr
    b <- doesFileExist path
    if b
        then do oldH <- B.readFile path
                return $ newH == oldH
        else return False

writeHash :: HsbinEnv -> HsbinScript -> Hash -> IO ()
writeHash henv hscr hash = do
    let path = hsHashPath henv hscr
    B.writeFile path hash

hscrHash :: HsbinScript -> IO Hash
hscrHash hscr = hashlazy <$> LB.readFile (hsPath hscr)
