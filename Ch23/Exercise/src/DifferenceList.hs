module DifferenceList where

import Control.Applicative
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 5 == 0 = "Buzz"
    | n `mod` 3 == 0 = "Fizz"
    | otherwise = show n

-- fizzbuzzList :: [Integer] -> [String]
-- fizzbuzzList list = 
--     let dlist =
--             execState (mapM_ addResult list) DL.empty 
--     in DL.apply dlist []

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list =
    execState (mapM_ addResult list) DL.empty

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo s e = execState (mapM_ addResult' [s..e]) []

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)

addResult' :: Integer -> State [String] ()
addResult' n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

hello :: IO ()
hello = mapM_ putStrLn $ fizzbuzzList [1..100]

hello2 :: IO ()
hello2 = mapM_ putStrLn $ reverse $ fizzbuzzFromTo 1 100

--

