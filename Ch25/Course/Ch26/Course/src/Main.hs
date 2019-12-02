{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  putStrLn "hello world"

-----------------------------------------------------------

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure . pure $ x)
  -- (MaybeT fab) <*> (MaybeT mma) = MaybeT $ liftA2 (<*>) fab mma
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

-----------------------------------------------------------

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure . pure $ x)
  (EitherT fab) <*> (EitherT mma) = EitherT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      Left e -> return $ Left e
      Right a -> runEitherT (f a)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ f <$> ma
  where
    f (Left x) = Right x
    f (Right x) = Left x

hello :: Int -> String
hello x = case () of _  | x >= 0 -> "hello"
                        | otherwise -> "world"

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT me) = me >>= (\e -> case e of
                                                    Left x -> f x
                                                    Right x -> g x)

------------------------------------------------------------------

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT ma) = ReaderT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT $ (pure . pure) x
  (ReaderT mab) <*> (ReaderT ma) = ReaderT $ (<*>) <$> mab <*> ma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

------------------------------------------------------------------

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ (fmap . fmap) (p f) sma
    where
      p f (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (f, _) <- smf s
    (a, _) <- sma s
    return (f a, s)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, _) <- sma s
    (runStateT . f) a s

------------------------------------------------------------------



------------------------------------------------------------------



------------------------------------------------------------------



------------------------------------------------------------------



------------------------------------------------------------------



------------------------------------------------------------------



------------------------------------------------------------------



------------------------------------------------------------------



------------------------------------------------------------------