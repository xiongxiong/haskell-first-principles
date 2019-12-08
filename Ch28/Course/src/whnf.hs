module WHNF where

import Criterion.Main
import Debug.Trace

myList :: [Int]
myList = trace "myList was evaluated" ([1..9999] ++ [undefined])

infix 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n 
  | n < 0 = Nothing 
  | otherwise = foldr (\x r k -> case k of
                                  0 -> Just x
                                  _ -> r (k - 1))
                      (const Nothing) xs n

main :: IO ()
main = defaultMain
    [
        bench "index list 9999" $ whnf (myList !!) 9999,
        bench "index list maybe index 9999" $ nf (myList !?) 9999
    ]

------------------------------------------------------------------

myList2 :: [Int]
myList2 = [1..9999]
-- myList2 = (undefined : [2..9999])
-- myList2 = (undefined : undefined)
-- myList2 = undefined

main2 :: IO ()
main2 = defaultMain     
    [
        -- bench "map list 9999" $ whnf (map (+1)) myList2
        bench "map list 9999" $ nf (map (+1)) myList2
    ]