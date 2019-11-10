module Apl2 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

listPure :: a -> [a]
listPure x = [x]

listAppl :: [a -> b] -> [a] -> [b]
listAppl (f : fs) xs = (fmap f xs) <> (listAppl fs xs)

ioPure :: a -> IO a
ioPure = return

ioAppl :: IO (a -> b) -> IO a -> IO b
ioAppl oof ooa = do
    f <- oof
    a <- ooa
    return (f a)

tuplePure :: Monoid a => b -> (a, b)
tuplePure x = (mempty, x)

tupleAppl :: Monoid e => (e, a -> b) -> (e, a) -> (e, b)
tupleAppl (e1, f) (e2, x) = (e1 <> e2, f x)

funcPure :: a -> (->) e a 
funcPure = const

funcAppl :: (e -> a -> b) -> (e -> a) -> (e -> b)
funcAppl f g x = f x (g x)

--

data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

--

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (EqProp a, EqProp b) => EqProp (Two a b) where
    (Two a b) =-= (Two a' b') = a =-= a' .&. b =-= b'

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (Two a1 f) <*> (Two a2 x) = Two (a1 <> a2) (f x)

--

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (EqProp a, EqProp b, EqProp c) => EqProp (Three a b c) where
    (Three a b c) =-= (Three a' b' c') = a =-= a' .&. b =-= b' .&. c =-= c'

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (Three a b f) <*> (Three a' b' x) = Three (a <> a') (b <> b') (f x)

--

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (EqProp a, EqProp b) => EqProp (Three' a b) where
    (Three' a b1 b2) =-= (Three' a' b1' b2') = a =-= a' .&. b1 =-= b1' .&. b2 =-= b2'

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where 
    pure x = Three' mempty x x
    (Three' a f1 f2) <*> (Three' a' x1 x2) = Three' (a <> a') (f1 x1) (f2 x2)

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (EqProp a, EqProp b, EqProp c, EqProp d) => EqProp (Four a b c d) where 
    (Four a b c d) =-= (Four a' b' c' d') = a =-= a' .&. b =-= b' .&. c =-= c' .&. d =-= d'

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (Four a b c f) <*> (Four a' b' c' x) = Four (a <> a') (b <> b') (c <> c') (f x)

--

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = (Four') a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure = (Four') mempty mempty mempty
    (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' x) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f x)

--



