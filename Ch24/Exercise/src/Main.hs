{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Applicative
import           Text.Trifecta
import Text.Printf
import           Data.Maybe                     ( isNothing )
import Data.List
import        Test.QuickCheck
import Text.RawString.QQ


main :: IO ()
main = do
  putStrLn "hello world"

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
parseDigit = oneOf ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] <|> fail "expected: integer"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

---------------------------------------------------------------------

base10Integer' :: Parser Integer
base10Integer' = read <$> do
  h <- hyphenOrNot
  case h of
    Nothing -> some parseDigit
    Just x -> (:) <$> return x <*> some parseDigit

---------------------------------------------------------------------

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = 
  let p = sequence $ parseDigit <$ [1..3]
  in read <$> (try p <|> try (char '(' *> p <* char ')') <|> try (char '1' *> char '-' *> p))

parseExchange :: Parser Exchange
parseExchange = 
  let p = sequence $ parseDigit <$ [1..3]
  in read <$> (try p <|> try (char '-' *> p) <|> try (char ' ' *> p))

parseLineNumber :: Parser LineNumber
parseLineNumber = 
  let p = sequence $ parseDigit <$ [1..4]
  in read <$> (try p <|> try (char '-' *> p))

parsePhone :: Parser PhoneNumber
parsePhone = PhoneNumber <$> parseNumberingPlanArea <*> parseExchange <*> parseLineNumber

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
  arbitrary = frequency $ (,) 1 . return . Year <$> [2020..2025]

newtype Month = Month Integer

instance Show Month where
  show (Month x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Month where
  arbitrary = frequency $ (,) 1 . return . Month <$> [1..12]

newtype Day = Day Integer

instance Show Day where
  show (Day x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Day where
  arbitrary = frequency $ (,) 1 . return . Day <$> [1..31]

newtype Hour = Hour Integer

instance Show Hour where
  show (Hour x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Hour where
  arbitrary = frequency $ (,) 1 . return . Hour <$> [1..23]

newtype Minute = Minute Integer

instance Show Minute where
  show (Minute x) = let s = show x in if length s == 1 then '0' : s else s

instance Arbitrary Minute where
  arbitrary = frequency $ (,) 1 . return . Minute <$> [1..59]

newtype Title = Title String

instance Show Title where
  show (Title x) = show x

instance Arbitrary Title where
  arbitrary = Title <$> frequency [
    (1, return "Breakfast"),
    (1, return "Sanitizing moisture collector"),
    (1, return "Exercising"),
    (1, return "Lunch"),
    (1, return "Programming"),
    (1, return "Commuting home in rover"),
    (1, return "Dinner"),
    (1, return "Shower"),
    (1, return "Read"),
    (1, return "Sleep")
    ]

newtype LogComment = LogComment String

instance Show LogComment where
  show (LogComment x) = "-- " ++ (show x)

instance Arbitrary LogComment where
  arbitrary = LogComment <$> frequency [
    (1, return "wheee a comment"),
    (1, return "not necessarily"),
    (1, return "headache"),
    (1, return "say something"),
    (1, return "something good")
    ]

parseLogComment :: Parser LogComment
-- parseLogComment = try $ LogComment <$> (spaces *> string "--" *> spaces *> many letter <* newline)
parseLogComment = try $ LogComment <$> (spaces *> string "--" *> spaces *> many (notChar '\n'))

parseMaybeLogComment :: Parser (Maybe LogComment)
parseMaybeLogComment = (Just <$> parseLogComment) <|> return Nothing

data LogDate = LogDate Year Month Day (Maybe LogComment)

instance Show LogDate where
  show (LogDate y m d mc) = printf "# %s-%s-%s %s" (show y) (show m) (show d) (case mc of Nothing -> ""; Just c -> show c)

instance Arbitrary LogDate where
  arbitrary = LogDate <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

parseLogDate :: Parser LogDate
parseLogDate = LogDate <$> (Year <$> (char '#' *> spaces *> natural)) <*> (Month <$> (char '-' *> natural)) <*> (Day <$> (char '-' *> natural)) <*> parseMaybeLogComment

data LogTime = LogTime Hour Minute

instance Show LogTime where
  show (LogTime h m) = printf "%s:%s" (show h) (show m)

instance Arbitrary LogTime where
  arbitrary = LogTime <$> arbitrary <*> arbitrary

parseLogTime :: Parser LogTime
parseLogTime = LogTime <$> (Hour <$> natural) <*> (Minute <$> (char ':' *> natural))

data LogActivity = LogActivity LogTime Title (Maybe LogComment)

instance Show LogActivity where
  show (LogActivity t a mc) = printf "%s %s %s" (show t) (show a) (case mc of Nothing -> ""; Just c -> show c)

instance Arbitrary LogActivity where
  arbitrary = LogActivity <$> arbitrary <*> arbitrary <*> arbitrary

parseLogActivity :: Parser LogActivity
parseLogActivity = LogActivity <$> parseLogTime <*> (Title <$> (spaces *> many (letter <|> char ' '))) <*> parseMaybeLogComment

data LogDay = LogDay LogDate [LogActivity]

instance Show LogDay where
  show (LogDay d as) = concat [(show d), "\n", (concat $ (++ "\n") . show <$> as)]

instance Arbitrary LogDay where
  -- arbitrary = LogDay <$> arbitrary <*> (sequence $ arbitrary <$ [1..10])
  arbitrary = LogDay <$> arbitrary <*> (sequence $ replicate 10 arbitrary)

parseLogDay :: Parser LogDay
parseLogDay = LogDay <$> (parseLogDate <* spaces) <*> many (parseLogActivity <* spaces)

data Log = Log [LogComment] [LogDay]

instance Show Log where
  show (Log cs ds) = concat [(concat $ (++ "\n") . show <$> cs), "\n", (concat $ (++ "\n") . show <$> ds)]

instance Arbitrary Log where
  arbitrary = Log <$> (frequency $ (,) 1 . sequence . (flip replicate) arbitrary <$> [1..3]) <*> (sequence $ replicate 2 arbitrary)

parseLog :: Parser Log
parseLog = Log <$> many (parseLogComment <* spaces) <*> many (parseLogDay <* spaces)

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------
