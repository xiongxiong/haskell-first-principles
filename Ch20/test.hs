import Data.Foldable
import Data.Monoid

elem' :: (Eq a, Foldable t) => a -> t a -> Bool
elem' a ta = getAny $ fold' $ fmap (Any.(==a)) (toList' ta)

sum' :: (Num a, Foldable t) => t a -> a
sum' ta = getSum $ fold' $ fmap Sum (toList' ta)

product' :: (Num a, Foldable t) => t a -> a
product' ta = getProduct $ fold' $ fmap Product (toList' ta)

minimum' :: (Ord a, Foldable t) => t a -> a 
minimum' ta = foldr1 min (toList' ta)

maximum' :: (Ord a, Foldable t) => t a -> a 
maximum' ta = foldr1 max (toList' ta)

null' :: (Foldable t) => t a -> Bool 
null' ta = (length' ta) == 0

length' :: (Foldable t) => t a -> Int
length' ta = foldr (\_ z -> z + 1) 0 (toList' ta)

toList' :: (Foldable t) => t a -> [a]
toList' ta = foldr (:) [] ta

fold' :: (Monoid m, Foldable t) => t m -> m 
fold' tm = foldr mappend mempty tm

foldMap' :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap' f ta = foldr (\a z -> mappend (f a) z) mempty ta
