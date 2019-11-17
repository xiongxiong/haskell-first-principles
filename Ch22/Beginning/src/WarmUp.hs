{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WarmUp where

import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
-- tupled = (,) <$> rev <*> cap
-- tupled = do
--     r <- rev
--     c <- cap
--     return (r, c)
tupled = rev >>= (\r -> cap >>= (\c -> return (r,c)))

--

newtype Reader r a =
    Reader {runReader :: r -> a}

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader rf) (Reader ra) = Reader $ \r -> rf r (ra r)

instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r


--

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName,
    dogName :: DogName,
    address :: Address
    } deriving (Eq, Show)

data Dog = Dog {
    dogName :: DogName,
    dogAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog ((dogName :: Person -> DogName) p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> (dogName :: Person -> DogName) <*> address

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
-- getDogR' = Dog <$->> dogName <*->> address
getDogR' = liftA2 Dog (dogName :: Person -> DogName) address

--

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

--

foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot = (,) <$> map (+1) <*> length

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

--

-- getDogRM :: Person -> Dog
-- getDogRM = do
--     name <- dogName :: Person -> DogName
--     addy <- address
--     return $ Dog name addy
getDogRM :: Reader Person Dog
getDogRM = dName >>= (\name -> dAddress >>= (\addy -> return $ Dog name addy))

dName :: Reader Person DogName
dName = Reader (dogName :: Person -> DogName)

dAddress :: Reader Person Address
dAddress = Reader address


