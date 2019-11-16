{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
    (S x y) =-= (S p q) = (=-=) <$> x <*> p .&. (y =-= q)

instance Foldable n => Foldable (S n) where
    foldMap f (S x y) = let p = foldMap f x in p <> f y

instance Functor n => Functor (S n) where
    fmap f (S x y) = S (fmap f x) (f y)

instance Traversable n => Traversable (S n) where
    traverse f (S x y) = S <$> traverse f x <*> f y

sampleS = sample' (arbitrary :: Gen (S [] Int))

--

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency [(1, return Empty), (1, Leaf <$> arbitrary), (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node a b c) = Node (fmap f a) (f b) (fmap f c)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node a b c) = foldMap f a <> f b <> foldMap f c

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node a b c) = Node <$> traverse f a <*> f b <*> traverse f c
