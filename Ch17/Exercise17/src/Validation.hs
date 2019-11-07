module Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation err a = Failure' err | Success' a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' x) = Success' (f x)

instance Monoid e => Applicative (Validation e) where
    pure = Success'
    (Failure' f) <*> (Success' s) = Failure' f
    (Failure' f) <*> (Failure' g) = Failure' (f <> g)
    (Success' s) <*> (Failure' f) = Failure' f
    (Success' s) <*> (Success' t) = Success' (s t)

validToEither :: Validation e a -> Either e a
validToEither (Failure' err) = Left err
validToEither (Success' a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure' err
eitherToValid (Right a) = Success' a

data Errors =  DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)
