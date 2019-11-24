{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal 
    char '/'
    denominator <- decimal
    return (numerator % denominator)

type IntegerOrFraction = Either Integer Rational

integerOrFraction :: Parser IntegerOrFraction
integerOrFraction = (Right <$> try parseFraction) <|> (Left <$> try decimal)

testIntegerOrFraction :: IO ()
testIntegerOrFraction = do
    let parse = parseString integerOrFraction mempty
    print $ parse alsoBad
    print $ parse shouldWork
    print $ parse shouldAlsoWork

hello :: IO ()
hello = do
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
    let parseFraction' = parseString virtuousFraction mempty
    print $ parseFraction' badFraction
    print $ parseFraction' alsoBad
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork

            