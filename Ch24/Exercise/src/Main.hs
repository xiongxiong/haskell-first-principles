{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Applicative
import           Text.Trifecta
import           Text.Printf
import           Data.Maybe                     ( isNothing )
import           Data.List
import           Test.QuickCheck
import           Text.RawString.QQ
import           Control.Monad
import           Data.Word
import           Text.Parser.LookAhead
import           Data.Char
import           Data.Functor

main :: IO ()
main = do
  putStrLn "hello world"

trim :: String -> String
-- trim = dropWhileEnd isSpace . dropWhile isSpace
trim = (.) <$> dropWhile <*> dropWhileEnd $ isSpace

---------------------------------------------------------------------

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

numberOrString :: Parser NumberOrString
numberOrString = NOSS <$> some letter <|> NOSI <$> natural

type HyphenOrNot = Maybe Char

hyphenOrNot :: Parser HyphenOrNot
hyphenOrNot = Just <$> char '-' <|> return Nothing

type CharOrNot = Maybe Char

charOrNot :: Char -> Parser CharOrNot
charOrNot c = Just <$> char c <|> return Nothing

type Major = Integer
type Minor = Integer
type Patch = Integer

type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _     <- char '.'
  minor <- integer
  _     <- char '.'
  patch <- integer
  h1    <- hyphenOrNot
  if isNothing h1
    then return $ SemVer major minor patch [] []
    else do
      release <- many (numberOrString <* charOrNot '.')
      h2      <- hyphenOrNot
      if isNothing h2
        then return $ SemVer major minor patch release []
        else do
          metadata <- many (numberOrString <* charOrNot '.')
          return $ SemVer major minor patch release metadata

---------------------------------------------------------------------

parseDigit :: Parser Char
parseDigit = oneOf ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']
  <|> fail "expected: integer"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

---------------------------------------------------------------------

base10Integer' :: Parser Integer
base10Integer' = read <$> do
  h <- hyphenOrNot
  case h of
    Nothing -> some parseDigit
    Just x  -> (:) <$> return x <*> some parseDigit

---------------------------------------------------------------------

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea =
  let p = sequence $ parseDigit <$ [1 .. 3]
  in  read
        <$> (try p <|> try (char '(' *> p <* char ')') <|> try
              (char '1' *> char '-' *> p)
            )

parseExchange :: Parser Exchange
parseExchange =
  let p = sequence $ parseDigit <$ [1 .. 3]
  in  read <$> (try p <|> try (char '-' *> p) <|> try (char ' ' *> p))

parseLineNumber :: Parser LineNumber
parseLineNumber =
  let p = sequence $ parseDigit <$ [1 .. 4]
  in  read <$> (try p <|> try (char '-' *> p))

parsePhone :: Parser PhoneNumber
parsePhone =
  PhoneNumber <$> parseNumberingPlanArea <*> parseExchange <*> parseLineNumber

---------------------------------------------------------------------

exampleLog :: String
exampleLog = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

newtype Year = Year Integer

instance Show Year where
  show (Year x) = show x

instance Arbitrary Year where
  arbitrary = frequency $ (,) 1 . return . Year <$> [2020 .. 2025]

newtype Month = Month Integer

instance Show Month where
  show (Month x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Month where
  arbitrary = frequency $ (,) 1 . return . Month <$> [1 .. 12]

newtype Day = Day Integer

instance Show Day where
  show (Day x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Day where
  arbitrary = frequency $ (,) 1 . return . Day <$> [1 .. 31]

newtype Hour = Hour Integer

instance Show Hour where
  show (Hour x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Hour where
  arbitrary = frequency $ (,) 1 . return . Hour <$> [1 .. 23]

newtype Minute = Minute Integer

instance Show Minute where
  show (Minute x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Minute where
  arbitrary = frequency $ (,) 1 . return . Minute <$> [1 .. 59]

newtype Title = Title String

instance Show Title where
  show (Title x) = show x

instance Arbitrary Title where
  arbitrary = Title <$> frequency
    [ (1, return "Breakfast")
    , (1, return "Sanitizing moisture collector")
    , (1, return "Exercising")
    , (1, return "Lunch")
    , (1, return "Programming")
    , (1, return "Commuting home in rover")
    , (1, return "Dinner")
    , (1, return "Shower")
    , (1, return "Read")
    , (1, return "Sleep")
    ]

newtype LogComment = LogComment String

instance Show LogComment where
  show (LogComment x) = "-- " ++ (show x)

instance Arbitrary LogComment where
  arbitrary = LogComment <$> frequency
    [ (1, return "wheee a comment")
    , (1, return "not necessarily")
    , (1, return "headache")
    , (1, return "say something")
    , (1, return "something good")
    ]

parseLogComment :: Parser LogComment
parseLogComment =
  try
    $   LogComment
    <$> (spaces *> string "--" *> whiteSpace *> (trim <$> many (notChar '\n')))
    <?> "LogComment"

data LogDate = LogDate Year Month Day (Maybe LogComment)

instance Show LogDate where
  show (LogDate y m d mc) = printf
    "# %s-%s-%s %s"
    (show y)
    (show m)
    (show d)
    (case mc of
      Nothing -> ""
      Just c  -> show c
    )

instance Arbitrary LogDate where
  arbitrary = LogDate <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

parseLogDate :: Parser LogDate
parseLogDate =
  try
    $   LogDate
    <$> (Year <$> (spaces *> char '#' *> whiteSpace *> natural))
    <*> (Month <$> (char '-' *> natural))
    <*> (Day <$> (char '-' *> natural))
    <*> optional parseLogComment
    <?> "LogDate"

data LogTime = LogTime Hour Minute

instance Show LogTime where
  show (LogTime h m) = printf "%s:%s" (show h) (show m)

instance Arbitrary LogTime where
  arbitrary = LogTime <$> arbitrary <*> arbitrary

parseLogTime :: Parser LogTime
parseLogTime =
  try
    $   LogTime
    <$> (Hour <$> (spaces *> natural))
    <*> (Minute <$> (char ':' *> natural))
    <?> "LogTime"

data LogActivity = LogActivity LogTime Title (Maybe LogComment)

instance Show LogActivity where
  show (LogActivity t a mc) = printf
    "%s %s %s"
    (show t)
    (show a)
    (case mc of
      Nothing -> ""
      Just c  -> show c
    )

instance Arbitrary LogActivity where
  arbitrary = LogActivity <$> arbitrary <*> arbitrary <*> arbitrary

parseLogActivity :: Parser LogActivity
parseLogActivity =
  try
    $   LogActivity
    <$> parseLogTime
    <*> (   Title
        <$> (whiteSpace *> manyTill
              anyChar
              (lookAhead (string "--") <|> (pure <$> newline))
            )
        )
    <*> (pure <$> try parseLogComment <|> pure Nothing)
    <?> "LogActivity"

data LogDay = LogDay LogDate [LogActivity]

instance Show LogDay where
  show (LogDay d as) =
    concat [(show d), "\n", (concat $ (++ "\n") . show <$> as)]

instance Arbitrary LogDay where
  -- arbitrary = LogDay <$> arbitrary <*> (sequence $ arbitrary <$ [1..10])
  arbitrary = LogDay <$> arbitrary <*> replicateM 10 arbitrary

parseLogDay :: Parser LogDay
parseLogDay =
  try
    $   LogDay
    <$> parseLogDate
    <*> many (parseLogActivity <* spaces)
    <?> "LogDay"

data Log = Log [LogComment] [LogDay]

instance Show Log where
  show (Log cs ds) = concat
    [(concat $ (++ "\n") . show <$> cs), "\n", concat $ (++ "\n") . show <$> ds]

instance Arbitrary Log where
  arbitrary =
    Log
      <$> frequency ((,) 1 . sequence . flip replicate arbitrary <$> [1 .. 3])
      <*> replicateM 2 arbitrary

parseLog :: Parser Log
parseLog = try $ Log <$> many parseLogComment <*> many parseLogDay <?> "Log"

parseok = parseString
  (manyTill
    anyChar
    (try $ whiteSpace <* ((string "--" <* whiteSpace) <|> (pure <$> newline)))
  )
  mempty
  "hello --\n"
parseaa = parseString
  (manyTill (token anyChar) (try $ lookAhead $ string "--") <* string "--")
  mempty
  "hello -- world"
parsebb = parseString (whiteSpace >> string "--") mempty " --"
    
---------------------------------------------------------------------

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord)

instance Show IPAddress where
  -- show (IPAddress l) = "IPAddress " ++ show l
  show (IPAddress l) = "IPAddress " ++ (p . p1 $ ([], l))
    where p1 (ds, x)
            | x == 0 = ds
            | otherwise = p1 (mod x (2 ^ 8) : ds, div x (2 ^ 8))
          p ds = intercalate "." $ show <$> (replicate (4 - length ds) 0 ++ ds)

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  xs <- replicateM 4 (decimal <* skipOptional dot)
  if all (<= 255) xs
    then return $ IPAddress $ fromInteger $ fst $ foldr
      (\x (a, i) -> (a + x * i, i * 2 ^ 8))
      (0, 1)
      xs
    else fail "invalid ipAddress"

---------------------------------------------------------------------

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

instance Show IPAddress6 where
  -- show (IPAddress6 h l) = "IPAddress6 " ++ show (toInteger l + toInteger h * 2 ^ 64)
  show (IPAddress6 h l) = "IPAddress6 " ++ (p . toInteger) h ++ ":" ++ (p . toInteger) l
    where 
          p1 (cs, x)
            | x == 0 = cs
            | otherwise = p1 (mod x (2 ^ 4) : cs, div x (2 ^ 4))
          p2 xs = replicate (16 - length xs) '0' ++ (toUpper . intToDigit . fromIntegral <$> xs)
          p3 (cs, xs)
            | null xs = cs
            | otherwise = p3 (take 4 xs : cs, drop 4 xs)
          p x = intercalate ":" . reverse . p3 $ ([], p2 $ p1 ([], x))

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  ss <- many ((some alphaNum <* skipOptional colon) <|> colon *> pure "")
  xs <- pure . concat . sequence $ fmap
    (\s -> case length s of
      0 -> return 0
      _ -> parseString hexadecimal mempty ('x' : s)
      )
    ss
  let ds = let len = length xs in p len
          where p len
                  | len == 8 = xs
                  | len > 8 = error "invalid ipAddress"
                  | len < 8 = let idx = elemIndex 0 xs in
                      case idx of
                        Nothing -> xs
                        Just idc -> let (h, l) = splitAt idc xs in h ++ replicate (8 - len) 0 ++ l
  let (h, l) = splitAt 4 ds
  let c = fromInteger . fst . foldr (\x (a, i) -> (a + x * i, i * 2 ^ 16))
                                    (0, 1)
  return $ IPAddress6 (c h) (c l)

parsecc = parseString (many ((some alphaNum <* skipOptional dot) <|> (dot *> pure ""))) mempty

---------------------------------------------------------------------

ip4To6 :: IPAddress -> IPAddress6
ip4To6 (IPAddress l) = IPAddress6 0 (fromIntegral l)

ip6To4 :: IPAddress6 -> IPAddress
ip6To4 (IPAddress6 h l)
  | h == 0 = if l < 2 ^ 32
                then IPAddress (fromIntegral l)
                else error "invalid"
  | otherwise = error "invalid"
---------------------------------------------------------------------

---------------------------------------------------------------------
