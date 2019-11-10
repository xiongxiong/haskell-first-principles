module Exercise where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad
import Control.Applicative

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

monadNope = quickBatch $ monad (undefined :: Nope (Int, Int, Int))

--

data PhEither b a = PhLeft a | PhRight b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhEither b a) where
    arbitrary = frequency [(1, PhLeft <$> arbitrary), (1, PhRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhEither b a) where
    (=-=) = eq

instance Functor (PhEither a) where
    fmap f (PhLeft a) = PhLeft $ f a
    fmap _ (PhRight a) = PhRight a

instance Applicative (PhEither a) where
    pure = PhLeft
    PhRight a <*> _ = PhRight a
    _ <*> PhRight a = PhRight a
    PhLeft f <*> PhLeft x = PhLeft (f x)

instance Monad (PhEither a) where
    PhRight a >>= _ = PhRight a
    PhLeft a >>= f = f a

monadEither = quickBatch $ monad (undefined :: PhEither String (Int, Int, Int))

--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    Identity f <*> x = fmap f x

instance Monad Identity where
    Identity a >>= f = f a

monadIdentity = quickBatch $ monad (undefined :: Identity (Int, Int, Int))

--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil), (1, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Semigroup (List a) where
    Nil <> ys = ys
    xs <> Nil = xs
    Cons x xs <> ys = Cons x $ xs <> ys

instance Monoid (List a) where
    mempty = Nil

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    -- Cons f fs <*> xs = fmap f xs <> (fs <*> xs)
    Cons f fs <*> xs = mappend (fmap f xs) $ fs <*> xs

instance Monad List where
    Nil >>= _ = Nil
    Cons x xs >>= f = f x <> (xs >>= f)

monadList = quickBatch $ monad $ (undefined :: List (Int, Int, Int))

--

j :: Monad m => m (m a) -> m a
j = join
    
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
