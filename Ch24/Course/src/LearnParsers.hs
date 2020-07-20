{-# LANGUAGE OverloadedStrings #-}

module LearnParsers where

import Control.Monad.IO.Class
import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneStr :: Parser String
oneStr = string "1"

oneStr' :: Parser String
oneStr' = oneStr >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

oneEOF :: Parser Char
oneEOF = one <* eof

-- meantime :: String -> IO ()
-- meantime s = (print . parseString (string "1") mempty $ s) >> (print . parseString (string "12") mempty $ s) >> (print . parseString (string "123") mempty $ s)

-- meantime' :: String -> [String] -> IO ()
-- meantime' s ts = foldr1 (>>) $ print <$> (parseString <$> (string <$> ts) <*> pure mempty <*> pure s)

meantime'' :: Parser [String]
meantime'' = do
    x <- string "1"
    y <- string "2"
    z <- string "3"
    pure [x, x <> y, x <> y <> z] <* eof

string' :: (CharParsing m, Monad m, MonadIO m) => String -> m String
string' s = sequence $ char <$> s

-- string' :: String -> Parser String
-- string' [] = pure ""
-- string' (x:xs) = do
--     c <- char x
--     -- cs <- string' xs
--     -- pure $ c : cs
--     (:) <$> pure c <*> string' xs

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

hello :: IO ()
hello = do
    pNL "stop:"
    testParse (stop :: Parser Char)
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'