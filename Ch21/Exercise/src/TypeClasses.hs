module TypeClasses where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show)

instance Functor (Either' a) where
    fmap _ (Left' x) = Left' x
    fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
    pure = Right'
    Left' e <*> _ = Left' e
    Right' f <*> r = fmap f r

instance Foldable (Either' a) where
    foldMap _ (Left' _) = mempty
    foldMap f (Right' y) = f y

    foldr _ z (Left' _) = z
    foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
    traverse _ (Left' x) = pure (Left' x)
    traverse f (Right' y) = Right' <$> f y

--

-- instance Functor ((,) a) where
--     fmap f (x, y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
--     pure x = (mempty, x)
--     (u, f) <*> (v, x) = (u `mappend` v, f x)

-- instance Foldable ((,) a) where
--     foldMap f (_, y) = f y
--     foldr f z (_, y) = f y z

-- instance Traversable ((,) a) where
--     traverse f (x, y) = (,) x <$> f y

--

newtype Identity' a = Identity' a
    deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity' a) where
    arbitrary = Identity' <$> arbitrary

instance Eq a => EqProp (Identity' a) where
    (=-=) = eq

instance Foldable Identity' where
    foldMap f (Identity' a) = f a

instance Functor Identity' where
    fmap f (Identity' a) = Identity' (f a)
   
instance Traversable Identity' where
    traverse f (Identity' a) = Identity' <$> f a

--

newtype Constant' a b =
    Constant' {getContant :: a} deriving (Eq, Ord, Show)

instance Monoid a => Arbitrary (Constant' a b) where
    arbitrary = return $ Constant' mempty

instance Eq a => EqProp (Constant' a b) where
    (=-=) = eq

instance Foldable (Constant' a) where
    foldMap _ _ = mempty

instance Functor (Constant' a) where
    fmap _ (Constant' a) = Constant' a

instance Traversable (Constant' a) where
    traverse _ (Constant' a) = pure (Constant' a)

--

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = frequency [(1, return Nada), (1, arbitrary)]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq
    
instance Foldable Optional where
    foldMap f Nada = mempty

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

--

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil), (1, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = f x <> foldMap f xs

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> (f x) <*> traverse f xs

--

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

--

data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

instance Foldable (Pair a) where
    foldMap f (Pair a b) = f b

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

--

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

instance Foldable (Big a) where
    foldMap f (Big a b c) = f b <> f c

instance Functor (Big a) where
    fmap f (Big a b c) = Big a (f b) (f c)

instance Traversable (Big a) where
    traverse f (Big a b c) = Big a <$> (f b) <*> f c

--

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq

instance Foldable (Bigger a) where
    foldMap f (Bigger a b c d) = f b <> f c <> f d

instance Functor (Bigger a) where
    fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Traversable (Bigger a) where
    traverse f (Bigger a b c d) = Bigger a <$> f b <*> f c <*> f d

--

