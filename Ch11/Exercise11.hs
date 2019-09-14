module Exercise11 where

import           Data.Char
import Data.List
import           Data.Maybe

cipher :: String -> String -> String
cipher message []      = message
cipher message keyword = zipWith f message keys
  where
    keys = fst (foldl conv ("", 0) message)
    conv (cs, count) ' ' = (cs ++ [' '], count)
    conv (cs, count) _ =
        (cs ++ [keyword !! mod count (length keyword)], count + 1)
    f c ' ' = c
    f c k =
        let newOrd = ord c + ord k - ord 'A'
        in  if newOrd > ord 'Z'
                then chr (ord 'A' + mod newOrd (ord 'Z') - 1)
                else chr newOrd

getCipher = cipher "MEET AT DAWN" "ALLY"

testCipher = if cipher "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
    then putStrLn "Get 'em, Tiger."
    else putStrLn "Bad news bears."

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf xx@(x : xs) (y : ys) =
    if x == y then isSubseqOf xs ys else isSubseqOf xx ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(c : cs) -> (w, toUpper c : cs)) . words

capitalizeWord :: String -> String
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph =
    unwords
        . fst
        . foldl
              (\(s, b) w ->
                  ( s ++ [if b then capitalizeWord w else w]
                  , case w of
                    [] -> False
                    _  -> last w == '.' 
                  )
              )
              ([], True)
        . words

------------------------------------------------------------

data DaPhone = DaPhone [(Char, String)]

daPhone :: DaPhone
daPhone = DaPhone
    [ ('1', "1")
    , ('2', "abc2")
    , ('3', "def3")
    , ('4', "ghi4")
    , ('5', "jkl5")
    , ('6', "mno6")
    , ('7', "pqrs7")
    , ('8', "tuv8")
    , ('9', "wxyz9")
    , ('*', "*^")
    , ('0', " +_0")
    , ('#', "#.,") ]

convo :: [String]
convo = 
    ["Wanna play 20 questions"
    ,"Ya"
    ,"U 1st haha"
    ,"Lol ok. Have u ever tasted alcohol"
    ,"Lol ya"
    ,"Wow ur cool haha. Ur turn"
    ,"Ok. Do u think I am pretty Lol"
    ,"Lol ya"
    ,"Just making sure rofl ur turn"]

type Digit = Char

type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone ms) ch = if isUpper ch
    then ('*', 1) : (se . toLower $ ch)
    else se ch
        where se d = fmap (\(c, j) -> (c, fromJust j)) . filter (\(_, q) -> isJust q) $ map (\(c, s) -> (c, elemIndex d s)) ms

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = foldMap (reverseTaps phone)

mostPopularLetter :: String -> Char
mostPopularLetter = fst . maxi . accu . (cellPhonesDead daPhone)
    where   m (DaPhone ms) = map (\(c, _) -> (c, 0)) ms
            accu :: [(Digit, Presses)] -> [(Digit, Presses)]
            accu = foldr (\(c, _) l -> foldr (\(c', count) n -> if c == c' then (c, count + 1) : n else (c', count) : n) [] l) (m daPhone)
            maxi ms = foldr (\(c, count) (c', count') -> if count > count' then (c, count) else (c', count')) (head ms) ms

coolestLtr :: [String] -> Char
coolestLtr = fst . maxi . accu . foldMap (cellPhonesDead daPhone)
    where   m (DaPhone ms) = map (\(c, _) -> (c, 0)) ms
            accu :: [(Digit, Presses)] -> [(Digit, Presses)]
            accu = foldr (\(c, _) l -> foldr (\(c', count) n -> if c == c' then (c, count + 1) : n else (c', count) : n) [] l) (m daPhone)
            maxi ms = foldr (\(c, count) (c', count') -> if count > count' then (c, count) else (c', count')) (head ms) ms

coolestWord :: [String] -> String
coolestWord = fst . maxi . accu . ws
    where   ws = foldMap words
            accu :: [String] -> [(String, Integer)]
            accu = foldr (\w m -> if w `elem` fmap fst m
                then foldr (\(w', co) n -> if w == w' then (w, co + 1) : n else (w', co) : n) [] m
                else (w, 1) : m) []
            maxi ms = foldr (\(c, count) (c', count') -> if count > count' then (c, count) else (c', count')) (head ms) ms

---------------------------------------------------------------------------

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b