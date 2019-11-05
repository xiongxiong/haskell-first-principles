module Main where

import Control.Applicative
import Data.List (elemIndex)
import Apl1
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "hello world"

f x = lookup x [
  (3, "hello"),
  (4, "julie"),
  (5, "kbai")
  ]

g y = lookup y [
  (7, "sup?"),
  (8, "chris"),
  (9, "aloha")
  ]

h z = lookup z [(2,3), (5,6), (7,8)]

m x = lookup x [(4,10), (8,13), (1,9001)]

added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1..3] [4..6])

y :: Maybe Integer
y = lookup 3 $ zip [1..3] [4..6]

z :: Maybe Integer
z = lookup 2 $ zip [1..3] [4..6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x'' :: Maybe Int
x'' = elemIndex 3 [1..5]

y'' :: Maybe Int
y'' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x'' <*> y''

xs = [1..3]
ys = [4..6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y'

newtype Identity' a = Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' (f x)

instance Applicative Identity' where
  pure = Identity'
  (<*>) (Identity' f) (Identity' x) = Identity' (f x) 

newtype Constant' a b = 
  Constant' { getConstant' :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
  fmap _ (Constant' a) = Constant' a

instance Monoid a => Applicative (Constant' a) where
  pure _ = Constant' mempty
  (<*>) (Constant' a) (Constant' c) = Constant' (a <> c)

--

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = 
  if (length s) > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
-- mkPerson n a = 
--   case mkName n of
--     Nothing -> Nothing
--     Just n' -> 
--       case mkAddress a of
--         Nothing -> Nothing
--         Just a' -> Just $ Person n' a'
mkPerson n a = Person <$> mkName n <*> mkAddress a

--

data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n 
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString name' age' weight' =
--   case noEmpty name' of
--     Nothing -> Nothing
--     Just nammy -> 
--       case noNegative age' of
--         Nothing -> Nothing
--         Just agey ->
--           case noNegative weight' of
--             Nothing -> Nothing
--             Just weighty ->
--               Just (Cow nammy agey weighty)
cowFromString name' age' weight' =
  Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'
-- cowFromString name' age' weight' =
--   liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')


