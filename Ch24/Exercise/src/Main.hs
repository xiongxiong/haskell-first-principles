module Main where

import           Control.Applicative
import           Text.Trifecta
import           Data.Maybe                     ( isNothing )


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

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------

---------------------------------------------------------------------
