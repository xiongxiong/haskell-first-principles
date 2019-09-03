import Data.List

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True True = [True]
eftBool _ _ = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd x y
    | x == y = [x]
    | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt x y
    | x == y = [x]
    | x < y = x : eftInt (x + 1) y
    | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar x y
    | x == y = [x]
    | x < y = x : (eftChar (toEnum (fromEnum x + 1)) y)
    | otherwise = []

myWords :: String -> [String]
myWords [] = []
myWords s = takeWhile (/= ' ') s : myWords (dropWhile (== ' ') $ dropWhile (/= ' ') s)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines s = takeWhile (/= '\n') s : myLines (dropWhile (== '\n') $ dropWhile (/= '\n') s)

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myAnd :: [Bool] -> Bool
myAnd [] = True
-- myAnd (x:xs) =
    -- if x == False
    -- then False
    -- else myAnd xs
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
-- myOr (x:xs) =
    -- if x == True
    -- then True
    -- else myOr xs
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem j xs = any (\x -> x == j) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs) = max f x xs
    where   max :: (a -> a -> Ordering) -> a -> [a] -> a
            max _ x [] = x
            max f x (y:ys) = max f (case f x y of GT -> x; EQ -> x; LT -> y) ys

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs) = min f x xs
    where   min :: (a -> a -> Ordering) -> a -> [a] -> a 
            min _ x [] = x
            min f x (y:ys) = min f (case f x y of LT -> x; EQ -> x; GT -> y) ys
            
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
