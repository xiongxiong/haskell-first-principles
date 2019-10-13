module Using where

import Data.List (sort)
import Test.QuickCheck
import Type.Reflection

half x = x / 2

halfIdentity = (*2) . half

checkHalfIdentity :: Double -> Bool
checkHalfIdentity x = halfIdentity x == x

newtype ListOrder a = ListOrder {getList :: [a]} deriving (Show)

instance (Ord a, Arbitrary a) => Arbitrary (ListOrder a) where
    arbitrary = ListOrder <$> (sort <$> arbitrary) 

instance Foldable ListOrder where
    foldr f l (ListOrder m) = (foldr :: (a -> b -> b) -> b -> [a] -> b) f l m

listOrdered :: (Ord a) => ListOrder a -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where 
        go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

multiAssociative x y z = x * (y * z) == (x * y) * z

multiCommutative x y = x * y == y * x

myQuotRem x y = 
    y == 0 || (quot x y) * y + (rem x y) == x

myDivMod x y = 
    y == 0 || (div x y) * y + (mod x y) == x

caretAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

caretCommutative x y = x ^ y == y ^ x

reverseTwice :: Eq a => [a] -> Bool
reverseTwice x = (reverse . reverse) x == id x

instance Show (a -> b) where
    show f = "func"

-- dollarSign :: (CoArbitrary a, Arbitrary b, Eq b) => (a -> b) -> a -> Bool
dollarSign :: (Int -> Int) -> Int -> Bool
dollarSign f a = (f $ a) == f a

dotSign :: (Arbitrary a, CoArbitrary a, Arbitrary b, Eq b, CoArbitrary b, Arbitrary c, Eq c) => (b -> c) -> (a -> b) -> a -> Bool
dotSign f g x = (f . g) x == (\x -> f (g x)) x

check1 :: Eq a => [a] -> [a] -> Bool
check1 xs ys = foldr (:) xs ys == (++) xs ys

check2 :: [String] -> Bool
check2 xs = foldr (++) [] xs == concat xs

f (NonNegative n) (InfiniteList xs _) = length (take n xs) == n

g (NonZero x) = (read (show x)) == x

square x = x * x

squareIdentity x = (square . sqrt) x == x