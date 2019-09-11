
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (||) False . (fmap f)

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (||) False . (fmap (== a))
-- myElem a = any (== a)

myReverse :: [a] -> [a]
-- myReverse = foldl (flip (:)) []
-- myReverse = foldr (flip (++)) [] . fmap (replicate 1)
myReverse = foldr (flip (++)) [] . fmap (\x -> [x])

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs
