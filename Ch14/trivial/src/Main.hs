{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

main :: IO ()
main = sample trivialGen

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial 
trivialGen = return Trivial 

instance Arbitrary Trivial where 
    arbitrary = trivialGen

newtype Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = Identity <$> arbitrary 

instance Arbitrary a => Arbitrary (Identity a) where 
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenFloat :: Gen (Identity Float)
identityGenFloat = identityGen

data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  Pair a <$> arbitrary

pairGen1 :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen1 = Pair <$> arbitrary <*> arbitrary 

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = 
  First a | Second b
  deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

trueGen :: Gen Int
trueGen = coarbitrary True arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False arbitrary

