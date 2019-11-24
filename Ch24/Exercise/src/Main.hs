module Main where

import Control.Applicative
import Text.Trifecta


main :: IO ()
main = do
  putStrLn "hello world"

---------------------------------------------------------------------

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

numberOrString :: Parser NumberOrString
numberOrString = NOSS <$> some letter <|> NOSI <$> integer

type Major = Integer
type Minor = Integer
type Patch = Integer

type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  _ <- char '.'
  release <- many numberOrString
  _ <- char '.'
  metadata <- many numberOrString
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
