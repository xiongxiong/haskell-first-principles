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
