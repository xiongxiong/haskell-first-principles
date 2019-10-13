module Test13 where

import Control.Monad
import System.Exit
import Data.Char

twoo :: IO ()
twoo = do   c   <- getChar
            c'  <- getChar
            when (c == c') $ putStrLn "True"

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let line = map toLower $ filter isLetter line1
    case line == reverse line of
        True -> putStrLn "It's a palindrome!"
        False -> putStrLn "Nope" >>= const exitSuccess

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age 
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "input name: "
    name <- getLine 
    putStr "input age: "
    ageStr <- getLine
    let age = read ageStr :: Integer
    case mkPerson name age of
        Left invalid -> putStrLn $ "Something wrong: " ++ show invalid
        Right person -> putStrLn $ "Yay! Successfully got a person: " ++ show person