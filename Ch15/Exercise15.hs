-- import Data.Monoid hiding (<>)
import Data.Semigroup
import Test.QuickCheck hiding (Success, Failure)

semigroupAssoc :: (Eq m, Semigroup m)
    => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a `mappend` mempty) == a

-------------------------------------------------------------

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial
    
instance Monoid Trivial where 
    mempty = Trivial 
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = 
    Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

--------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where 
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where 
    mempty = Identity $ mempty 
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = frequency [(1, Identity <$> arbitrary)]

type IdenAssoc a = 
    Identity a -> Identity a -> Identity a -> Bool

checkIdentity :: IO ()
checkIdentity = do
    quickCheck (semigroupAssoc :: IdenAssoc String)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    
--------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where 
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = frequency [(1, Two <$> arbitrary <*> arbitrary)]

type TwoAssoc a b =
    Two a b -> Two a b -> Two a b -> Bool

checkTwo :: IO ()
checkTwo = do
    quickCheck (semigroupAssoc :: TwoAssoc [Int] String)
    quickCheck (monoidLeftIdentity :: Two [Int] String -> Bool)
    quickCheck (monoidRightIdentity :: Two [Int] String -> Bool)

-------------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c, Semigroup a, Semigroup b, Semigroup c) => Monoid (Three a b c) where 
    mempty = Three mempty mempty mempty 
    mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = frequency [(1, Three <$> arbitrary <*> arbitrary <*> arbitrary)]

type ThreeAssoc a b c = 
    Three a b c -> Three a b c -> Three a b c -> Bool

checkThree :: IO ()
checkThree = do
    quickCheck (semigroupAssoc :: ThreeAssoc Any All (Sum Int))
    quickCheck (monoidLeftIdentity :: Three Any All String -> Bool)
    quickCheck (monoidRightIdentity :: Three Any All String -> Bool)

--------------------------------------------------------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d, Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Monoid (Four a b c d) where 
    mempty = Four mempty mempty mempty mempty 
    mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = frequency [(1, Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)]

type FourAssoc a b c d = 
    Four a b c d -> Four a b c d -> Four a b c d -> Bool

checkFour :: IO ()
checkFour = do
    quickCheck (semigroupAssoc :: FourAssoc Any All (Sum Int) (Product Int))
    quickCheck (monoidLeftIdentity :: Four Any All [Int] String -> Bool)
    quickCheck (monoidRightIdentity :: Four Any All [Int] String -> Bool)

----------------------------------------------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where 
    (BoolConj False) <> _ = BoolConj False
    _ <> (BoolConj False) = BoolConj False
    _ <> _ = BoolConj True

instance Monoid BoolConj where 
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where 
    arbitrary = frequency [(1, BoolConj <$> arbitrary)]

type BoolConjAssoc =
    BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = do
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

------------------------------------------------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where 
    (BoolDisj True) <> _ = BoolDisj True 
    _ <> (BoolDisj True) = BoolDisj True 
    _ <> _ = BoolDisj False

instance Monoid BoolDisj where 
    mempty = BoolDisj False 
    mappend = (<>)

instance Arbitrary BoolDisj where 
    arbitrary = frequency [(1, BoolDisj <$> arbitrary)]

type BoolDisjAssoc =
    BoolDisj -> BoolDisj -> BoolDisj -> Bool 

checkBoolDisj :: IO ()
checkBoolDisj = do
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

--------------------------------------------------------

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where 
    (Fst a) <> (Snd b) = Snd b 
    (Fst a) <> (Fst b) = Fst a 
    (Snd a) <> _ = Snd a 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where 
    arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

type OrAssoc a b =
    Or a b -> Or a b -> Or a b -> Bool

checkOr :: IO ()
checkOr = 
    quickCheck (semigroupAssoc :: OrAssoc Any All)

--------------------------------------------------------

newtype Combine a b = Combine {unCombine :: (a -> b)}

instance Eq (Combine a b) where 
    (==) (Combine f) (Combine g) = True

instance Show (Combine a b) where 
    show (Combine f) = "Combine"

instance Semigroup b => Semigroup (Combine a b) where 
    (Combine f) <> (Combine g) = Combine $ \x -> (f x) <> (g x)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where 
    mempty = Combine $ \_ -> mempty
    mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where 
    arbitrary = frequency [(1, Combine <$> arbitrary)]

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool

checkCombine :: IO ()
checkCombine = do 
    quickCheck (semigroupAssoc :: CombineAssoc Int String)
    quickCheck (monoidLeftIdentity :: Combine Any All -> Bool)
    quickCheck (monoidRightIdentity :: Combine Any All -> Bool)

--------------------------------------------------------

newtype Comp a = Comp {unComp :: (a -> a)}

instance Eq (Comp a) where 
    (==) _ _ = True

instance Show (Comp a) where 
    show (Comp f) = "Comp"

instance Semigroup a => Semigroup (Comp a) where 
    (Comp (a)) <> (Comp (b)) = Comp $ (\x -> (a x) <> (b x)) 

instance (Monoid a, Semigroup a) => Monoid (Comp a) where 
    mempty = Comp $ (\_ -> mempty)
    mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where 
    arbitrary = frequency [(1, Comp <$> arbitrary)]

type CompAssoc a = Comp a -> Comp a -> Comp a -> Bool

checkComp :: IO ()
checkComp = do 
    quickCheck (semigroupAssoc :: CompAssoc String)
    quickCheck (monoidLeftIdentity :: Comp Any -> Bool)
    quickCheck (monoidRightIdentity :: Comp Any -> Bool)

-------------------------------------------------------------

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a =>
    Semigroup (Validation a b) where 
    (<>) (Failure a) (Failure b) = Failure $ a <> b
    (<>) (Success a) (Success b) = Success a
    (<>) (Failure _) (Success b) = Success b 
    (<>) (Success a) (Failure _) = Success a 

doValidation :: IO ()
doValidation = do 
    let failure :: String -> Validation String Int 
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

------------------------------------------------------------

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance (Semigroup a, Num s) => Semigroup (Mem s a) where 
    (Mem f) <> (Mem g) = Mem $ \x -> 
        let (a1, s1) = f x
            (a2, s2) = g x
        in (a1 <> a2, s1 + s2)

instance (Monoid a, Semigroup a, Num s) => Monoid (Mem s a) where 
    mempty = Mem $ \x -> (mempty, x)
    mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

checkMem :: IO ()
checkMem = do 
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft 
    print $ rmright 
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0


