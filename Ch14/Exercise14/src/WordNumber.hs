module WordNumber where

import Data.List (intersperse)
 
cattyConny :: String -> String -> String 
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny 

appedCatty = cattyConny "woops"
frappe = flippy "haha"

suma :: (Eq a, Num a) => a -> a
suma x = go x 0 
    where go x sum 
            | x == 0 = sum 
            | otherwise = go (x - 1) (sum + x)

multiBy :: (Eq a, Num a) => a -> a -> a
multiBy x y = go x y 0
    where go x y sum 
            | y == 0 = sum 
            | otherwise = go x (y - 1) (sum + x)

mc91 :: (Eq a, Num a, Ord a) => a -> a 
mc91 x 
    | x > 100 = x - 10 
    | otherwise = mc91 $ mc91 $ x + 11

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "-"

digits :: Int -> [Int]
digits x = go x []
    where go x xs
            | x < 10 = x:xs
            | otherwise = 
                let r = x `mod` 10
                    q = x `div` 10 
                in go q (r:xs)

wordNumber :: Int -> String 
wordNumber x = 
    let ns = digits x 
        ns' = intersperse (-1) ns                 
        ws = map digitToWord ns'
    in concat ws
    