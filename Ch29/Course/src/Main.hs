module Main where

import Control.Concurrent
import Debug.Trace
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.Time.Calendar
import Data.Time.Clock
import System.Random
import System.IO.Unsafe

myData :: IO (MVar Int)
myData = newEmptyMVar 

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero

--------------------------------------------------------------

blah :: IO String
blah = return "blah"

blah' = trace "outer blah" blah

woot :: IO String
woot = return $ (\x -> trace "inner trace" x) "woot"

hello :: IO ()
hello = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w

--------------------------------------------------------------

gimmeShelter :: Bool -> IO [Int]
gimmeShelter True = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]

aa :: (Int, Int) -> IO Int
aa x = (+) <$> randomRIO x <*> randomRIO x

--------------------------------------------------------------

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime 
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True -> return $ Left randomIO
    False -> return $ Right (putStrLn "no soup for you")

--------------------------------------------------------------

func1 :: IO ()
func1 = do
  mv <- newEmptyMVar 
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero

--------------------------------------------------------------

myData2 :: MVar Int
myData2 = unsafePerformIO newEmptyMVar

func2 :: IO ()
func2 = do
  putMVar myData2 0
  zero <- takeMVar myData2
  print zero