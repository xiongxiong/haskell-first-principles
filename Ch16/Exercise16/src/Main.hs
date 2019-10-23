module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

--

data CountingBad a =
  Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)