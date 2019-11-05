module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Semigroup a => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq

--

data CList a = Nil | Cons a (CList a) deriving (Eq, Show)

instance Semigroup (CList a) where
    (<>) Nil cons = cons
    (<>) (Cons x xs) cons = Cons x $ xs <> cons

instance Monoid (CList a) where
    mempty = Nil
    mappend = (<>)

instance Functor CList where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative CList where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) cons = mappend (fmap f cons) ((<*>) fs cons)

instance Foldable CList where
    foldr _ b Nil = b
    foldr f b (Cons h t) = f h (foldr f b t)

flatMap :: (a -> CList b) -> CList a -> CList b
flatMap f as = g clist
    where 
        clist = fmap f as
        g :: CList (CList a) -> CList a
        g Nil = Nil
        g (Cons Nil ys) = g ys
        g (Cons (Cons x xs) ys) = Cons x (g (Cons xs ys))
