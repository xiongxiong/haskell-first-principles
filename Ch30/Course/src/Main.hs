module Main where

---------------------------------------------------------------

-- import Control.Exception
-- import Data.Typeable

-- handler :: SomeException -> IO ()
-- -- handler (SomeException e) = do
-- --   print (typeOf e)
-- --   putStrLn ("We errored! It was: " ++ show e)
-- handler (SomeException e) = do
--   putStrLn ("Running main caused an error!\
--       \ It was: " ++ show e)
--   writeFile "bbb" "hi"

-- main :: IO ()
-- main = do
--   writeFile "zzz" "hi" `catch` handler

---------------------------------------------------------------

-- import Control.Exception
-- import System.Environment (getArgs)

-- willIFail :: Integer -> IO (Either ArithException ())
-- willIFail denom = try . print . (div 5) $ denom

-- onlyReportError :: Show e => IO (Either e a) -> IO ()
-- onlyReportError action = do
--   result <- action
--   case result of 
--     Left e -> print e
--     Right _ -> return ()

-- testDiv :: String -> IO ()
-- testDiv d = onlyReportError . willIFail $ read d

-- main :: IO ()
-- main = do
--   args <- getArgs
--   mapM_ testDiv args

---------------------------------------------------------------

-- canICatch :: Exception e => e -> IO (Either ArithException ())
-- canICatch e = try $ throwIO e

---------------------------------------------------------------

-- import Control.Concurrent (threadDelay)
-- import Control.Exception
-- import Control.Monad (forever)
-- import System.Random (randomRIO)

-- randomException :: IO ()
-- randomException = do
--   i <- randomRIO (1, 10 :: Int)
--   if i `elem` [1..9]
--     then throwIO DivideByZero
--     else throwIO StackOverflow

-- main :: IO ()
-- main = forever $ do
--   let tryS :: IO () -> IO (Either ArithException ())
--       -- tryS = try
--       tryS a = catch (a >>= return . Right) (return . Left)
--   _ <- tryS randomException
--   putStrLn "Live to loop another day!"
--   threadDelay (1 * 1000000)

---------------------------------------------------------------

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import System.IO

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "test.dat" WriteMode
  threadDelay 1500
  hPutStr h (replicate 100000000 '0' ++ "abc")
  hClose h

data PleaseDie = PleaseDie deriving (Show)

instance Exception PleaseDie

main :: IO ()
main = do
  threadId <- forkIO (mask_ openAndWrite)
  threadDelay 1000
  throwTo threadId PleaseDie