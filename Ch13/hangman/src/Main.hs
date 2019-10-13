module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

type WordList = [String]
-- newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 6

gameWords :: IO WordList
gameWords = filter gameLength <$> allWords
  where gameLength w = let l = length w in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
-- randomWord' = gameWords >>= randomWord
randomWord' = do
  gw <- gameWords
  randomWord gw

data Puzzle = Puzzle String [Maybe Char] String Int

instance Show Puzzle where
  show (Puzzle word discovered guessed wrongs) = intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed ++ " " ++ show (length word - wrongs) ++ " times left!"

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c = c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacterCorrect :: Puzzle -> Char -> Puzzle
fillInCharacterCorrect (Puzzle word filledInSoFar s _) c = Puzzle word newFilledInSoFar (c:s) 0
  where zipper guessed wordChar guessChar =
          if wordChar == guessed then Just wordChar else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

fillInCharacterWrong :: Puzzle -> Char -> Puzzle
fillInCharacterWrong (Puzzle word filledInSoFar s w) c = Puzzle word filledInSoFar (c:s) (w + 1)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly."
      return (fillInCharacterCorrect puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacterWrong puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed wrongs) = 
  when (length wordToGuess <= wrongs) $
    do 
      putStrLn "You Lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) = 
  when (all isJust filledInSoFar) $
    do 
      putStrLn "You win!"
      exitSuccess 

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"