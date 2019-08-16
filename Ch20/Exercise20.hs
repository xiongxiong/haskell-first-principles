import Data.Foldable
import Data.Monoid

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Two a b = Two a b 

instance Foldable (Two a) where 
    foldMap f (Two a b) = f b

data Three a b c = Three a b c 

instance Foldable (Three a b) where 
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b 

instance Foldable (Three' a) where 
    foldMap f (Three' a b c) = f b <> f c

data Four' a b = Four' a b b b 

instance Foldable (Four' a) where 
    foldMap f (Four' a b c d) = mconcat $ f <$> [b, c, d]


filterF :: (Applicative f, Foldable t, Monoid (f a))
    => (a -> Bool) -> t a -> f a
filterF f ta = foldMap pure $ foldMap (\a -> if f a then [a] else []) ta

