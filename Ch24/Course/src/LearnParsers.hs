module LearnParsers where

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

meantime :: IO ()
meantime = (,,)

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