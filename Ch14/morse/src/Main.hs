module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

main :: IO ()
main = do
  putStrLn "hello world"

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess

  line <- hGetLine stdin
  convertLine line

  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str)
          -> putStrLn (intercalate " " str)
        Nothing
          -> do
            putStrLn $ "ERROR:"