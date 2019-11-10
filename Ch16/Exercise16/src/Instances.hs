module Instances where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Law

check :: IO ()
check = do
    quickCheck checkIdentityForIdentity
    quickCheck checkComposeForIdentity

    quickCheck checkIdentityForPair
    quickCheck checkComposeForPair

    quickCheck checkIdentityForTwo
    quickCheck checkComposeForTwo

    quickCheck checkIdentityForThree
    quickCheck checkComposeForThree

    quickCheck checkIdentityForThree'
    quickCheck checkComposeForThree'

    quickCheck checkIdentityForFour
    quickCheck checkComposeForFour

    quickCheck checkIdentityForFour'
    quickCheck checkComposeForFour'

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

checkIdentityForIdentity :: Identity Int -> Bool
checkIdentityForIdentity = functorIdentity

checkComposeForIdentity :: Identity Int -> Bool
checkComposeForIdentity = functorCompose (+1) (*2)

--

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

checkIdentityForPair :: Pair Int -> Bool
checkIdentityForPair = functorIdentity

checkComposeForPair :: Pair Int -> Bool
checkComposeForPair = functorCompose (+1) (*2)

--

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

checkIdentityForTwo :: Two Int Int -> Bool
checkIdentityForTwo = functorIdentity

checkComposeForTwo :: Two Int Int -> Bool
checkComposeForTwo = functorCompose (+1) (*2)

--

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

checkIdentityForThree :: Three Int Int Int -> Bool
checkIdentityForThree = functorIdentity

checkComposeForThree :: Three Int Int Int -> Bool
checkComposeForThree = functorCompose (+1) (*2)

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

checkIdentityForThree' :: Three' String Int -> Bool
checkIdentityForThree' = functorIdentity

checkComposeForThree' :: Three' String Int -> Bool
checkComposeForThree' = functorCompose (+2) (*3)

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

checkIdentityForFour :: Four String Double Float Int -> Bool
checkIdentityForFour = functorIdentity

checkComposeForFour :: Four String Double Float Int -> Bool
checkComposeForFour = functorCompose (+2) (*3)

--

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

checkIdentityForFour' :: Four' String Int -> Bool
checkIdentityForFour' = functorIdentity

checkComposeForFour' :: Four' String Int -> Bool
checkComposeForFour' = functorCompose (+2) (*3)