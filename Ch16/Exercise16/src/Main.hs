module Main where

import ReplaceExperiment
import qualified Instances as I
import Test.QuickCheck
import Test.QuickCheck.Function

main :: IO ()
main = do
  putStr "replaceWithP' lms:  "
  print (replaceWithP' lms)

  putStr "liftedReplace lms:  "
  print (liftedReplace lms)

  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)

  putStr "twiceLifted lms:  "
  print (twiceLifted lms)

  putStr "twiceLifted' lms:  "
  print (twiceLifted' lms)

  putStr "thriceLifted lms: "
  print (thriceLifted lms)

  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

--

data CountingBad a =
  Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

--

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123" ++) (fmap show ioi))
    in fmap (*3) changed

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b) 

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap (g . f) x == fmap g (fmap f x)

--

checkFunctorIdentity :: [Int] -> Bool
checkFunctorIdentity x = functorIdentity x

checkFunctorCompose :: [Int] -> Bool
checkFunctorCompose x = functorCompose (+1) (*2) x

--

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

fc' = functorCompose' :: IntFC

--

