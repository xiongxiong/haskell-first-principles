module Queue where

import Criterion.Main

data Queue a =
    Queue {
        enqueue :: [a],
        dequeue :: [a]
    } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x (Queue en de) = Queue (x:en) de 

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue en []) = pop (Queue [] (reverse en))
pop (Queue en (x:xs)) = Just (x, Queue en xs)

q :: Queue Int
q = Queue [] []

pushQ :: Int -> Queue Int
pushQ n = foldr push q [1..n]

popQ :: Int -> Int
popQ n = foldr (\_ _ -> case pop q of Nothing -> 0; Just (x, _) -> x) 0 [1..n]

pushList :: a -> [a] -> [a]
pushList x xs = x:xs

popList :: [a] -> Maybe (a, [a])
popList [] = Nothing
popList (x:xs) = let (y, ys) = go xs (x, []) in Just (y, ys)
    where go [] (y, ys) = (y, ys)
          go (x:xs) (y, ys) = go xs (x, y:ys)

p :: [Int]
p = []

pushL :: Int -> [Int]
pushL n = foldr pushList p [1..n]

popL :: Int -> Int
popL n = foldr (\_ _ -> case popList p of Nothing -> 0; Just (x, _) -> x) 0 [1..n]

main :: IO ()
main = do
    defaultMain 
        [
            bench "push Queue" $ whnf pushQ 10000,
            bench "pop Queue" $ whnf popQ 10000,
            bench "push List" $ whnf pushL 10000,
            bench "pop List" $ whnf popL 10000
        ]