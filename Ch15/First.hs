import QuickCheck
import Optional
import Data.Monoid
import Test.QuickCheck
import Control.Monad

newtype First' a = 
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' { getFirst' = Nada }
    mappend (First' Nada) x = x
    mappend x _ = x

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = frequency [(1, return $ First' Nada), 
        (3, First' <$> (Only <$> arbitrary))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend) 
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
