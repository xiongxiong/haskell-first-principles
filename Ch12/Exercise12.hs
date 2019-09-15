module Exercise12 where

import Data.Maybe

notThe :: String -> Maybe String
notThe s 
    | s == "the" = Nothing
    | otherwise = Just s

replaceThe :: String -> String
replaceThe = unwords . fmap (fromMaybe "a") . map notThe . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . map notThe . words
    where   count :: [Maybe String] -> Integer
            count [] = 0
            count (x:xs) = snd $ foldl f (x, 0) xs

            startWithVowel :: String -> Bool
            startWithVowel [] = False
            startWithVowel (x:_) = elem x "aeiou"

            f :: (Maybe String, Integer) -> Maybe String -> (Maybe String, Integer)
            f (_, i) Nothing  = (Nothing, i)
            f (m, i) j@(Just s)  = if isNothing m && startWithVowel s then (j, i + 1) else (j, i)

countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` "aeiou")

newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = (\(a, b) -> if a > b then Nothing else Just (Word' w)) $ foldr (\c (a, b) -> if c `elem` vowels then (a + 1, b) else (a, b + 1)) ((0, 0) :: (Integer, Integer)) w

-- It's only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ Zero) = 1
natToInteger (Succ x) = natToInteger x + 1

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x == 0 = Just Zero
    | x < 0 = Nothing
    | otherwise = fmap Succ (integerToNat (x - 1))

-- Small library for Maybe

isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' _ = True

isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' b Nothing = b
fromMaybe' _ (Just a) = a

listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' (x:_) = Just x

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just x) = [x]

catMaybes' :: [Maybe a] -> [a]
catMaybes' = foldr (\x b -> case x of
    Nothing -> b
    Just j -> j : b) []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (\m n -> case n of
    Nothing -> Nothing
    Just n' -> case m of
        Nothing -> Nothing
        Just m' -> Just (m' : n')) (Just [])

-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x y -> case x of
    Left a -> a : y
    _ -> y) []

rights' :: [Either a b] -> [b]
rights' = foldr (\x y -> case x of
    Right b -> b : y
    _ -> y) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\x (m, n) -> case x of
    Left a -> (a : m, n)
    Right b -> (m, b : n)) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Write your own iterate and unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing -> []
    Just (j, k) -> j : myUnfoldr f k

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- Finally something other than a list!

data BinaryTree a =
    Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f s = case f s of
    Nothing -> Leaf
    Just (m, n, l) -> Node (unfold f m) n (unfold f l)

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder n = unfold (\x -> if x == n
    then Nothing 
    else Just (x + 1, x, x + 1)) 0
    