{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative hiding (Const)
import Control.Monad

main :: IO ()
main = do
  putStrLn "hello world"

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where 
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where 
    pure :: a -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    -- (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose a) = Compose <$> traverse (traverse f) a

-------------------------------------------------------------------

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (c -> d) -> p a c -> p a d
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

newtype Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

newtype SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

-------------------------------------------------------------------

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
  -- (IdentityT ma) >>= f = let aimb = runIdentityT . f =<< ma in IdentityT aimb
  -- (IdentityT ma) >>= f = 
  --   let aimb :: a
  --       aimb = fmap f ma 
  --   in undefined

-------------------------------------------------------------------
