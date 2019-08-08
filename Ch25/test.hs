newtype Identity a = Identity { runIdentity :: a }

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where 
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where 
    fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where 
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


