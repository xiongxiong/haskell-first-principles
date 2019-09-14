{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test11 where
    
import           Data.Int

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

-----------------------------------------------------------

data OperatingSystem =
    GnuPlusLinux | OpenBSDPlus | Mac | Windows
    deriving (Eq, Show)

data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgLang } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlus, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = Programmer <$> allOperatingSystems <*> allLanguages

------------------------------------------------------------

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a  = Node (insert' b left) a right
                              | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed"

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = postorder right ++ postorder left ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [3, 1, 2]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf                = b
foldTree f b (Node left a right) = foldTree f (foldTree f (f a b) left) right

testFoldTree :: IO ()
testFoldTree = if foldTree (+) 0 testTree == 6
    then putStrLn "Fold fine!"
    else putStrLn ("Bad news bears.")
