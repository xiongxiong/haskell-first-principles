import Data.Time

data DatabaseItem =   DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
-- filterDbDate xs = foldr (:) [] $ map (\(DbDate time) -> time) $ filter (\x -> case x of DbDate _ -> True; otherwise -> False) xs
filterDbDate = (foldr (:) []) . (map (\(DbDate time) -> time)) . filter (\x -> case x of DbDate _ -> True; otherwise -> False)

filterDbNumber :: [DatabaseItem] -> [Integer]
-- filterDbNumber xs = foldr (:) [] $ map (\(DbNumber num) -> num) $ filter (\x -> case x of DbNumber _ -> True; _ -> False) xs
filterDbNumber = (foldr (:) []) . (map (\(DbNumber num) -> num)) . filter (\x -> case x of DbNumber _ -> True; _ -> False)

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sum $ nums) / (fromIntegral . length $ nums)
    where nums = filterDbNumber xs

