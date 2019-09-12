{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Int

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

instance TooMany ((,) Int String) where
    tooMany (n, _) = n > 42

instance TooMany (Int, Int) where 
    tooMany (x, y) = x + y > 42

instance (Num a, Ord a) => TooMany (a, a) where 
    tooMany (x, y) = x * y > 42

-----------------------------------------------------------
data NumberOrBool =
        Numba Int8 
    |   BoolyBool Bool
    deriving (Eq, Show)

data Person =
    Person { name :: String, age :: Int } deriving (Eq, Show)

data Hason = 
    Hason { name :: String, age :: Int } deriving (Eq, Show)
