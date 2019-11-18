module Exercise where

import Control.Applicative

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State m) = State $ \s -> let (a, s') = m s in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    State f <*> State g = State $ \s -> let (f', _) = f s; (a, _) = g s in (f' a, s)

instance Monad (State s) where
    return = pure
    State f >>= g = State $ \s -> let (a, s') = f s in (runState (g a)) s'

get :: State s s
get = State $ (,) <$> id <*> id

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

