module NList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = Cons <$> arbitrary <*> arbitrary

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = take' 300 xs
            ys' = take' 300 ys

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)


newtype ZipList a = ZipList (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList a) where
    arbitrary = ZipList <$> arbitrary

instance Eq a => EqProp (ZipList a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList l) = xs in take' 3000 l
            ys' = let (ZipList l) = ys in take' 3000 l

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
    pure = ZipList <$> pure
    (ZipList listA) <*> (ZipList listB) = ZipList $ listA <*> listB 
