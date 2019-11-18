{-# LANGUAGE InstanceSigs #-}
module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import System.Random
import RandomExample

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) =
                        randomR (1, 6) gen
                in go (sum + die)
                        (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
    where
        go sum count gen
            | sum >= n = count
            | otherwise = 
                let (die, nextGen) = 
                        randomR (1, 6) gen
                in go (sum + die)
                        (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
    where
        go sum count dies gen 
            | sum >= n = (count, dies)
            | otherwise =
                let (die, nextGen) = 
                        randomR (1, 6) gen
                in go (sum + die) (count + 1) (intToDie die : dies) nextGen

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi m) = Moi $ \s -> let (a, s') = m s in (f a, s')

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    Moi f <*> Moi g = Moi $ \s -> let (f', _) = f s; (a, _) = g s in (f' a, s)

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    Moi f >>= g = Moi $ \s -> let (a, _) = f s in (runMoi (g a)) s

--

fizzBuzz :: Integer -> String
fizzBuzz n
        | n `mod` 15 == 0 = "FizzBuzz"
        | n `mod` 5 == 0 = "Buzz"
        | n `mod` 3 == 0 = "Fizz"
        | otherwise = show n

helloOne :: IO ()
helloOne = mapM_ (putStrLn . fizzBuzz) [1..100]

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

helloTwo :: IO ()
helloTwo = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

