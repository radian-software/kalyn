module Main where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           System.Posix.Files

import qualified ELF

main :: IO ()
main = do
  B.writeFile "main" (ELF.elfData $ toLazyByteString $ stringUtf8 "HELLOWORLD")
  setFileMode "main" 0o777
