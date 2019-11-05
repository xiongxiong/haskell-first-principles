module NList where

import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' n Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
    pure = flip Cons Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = (fmap f xs) `g` (fs <*> xs)
        where
            g :: List a -> List a -> List a
            g Nil xs = xs
            g xs Nil = xs
            g (Cons x xs) ys = Cons x $ g xs ys


newtype ZipList a = ZipList (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList l) = xs in take' 3000 l
            ys' = let (ZipList l) = ys in take' 3000 l

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
    pure = ZipList . pure
    (ZipList listA) <*> (ZipList listB) = ZipList $ listA <*> listB 
