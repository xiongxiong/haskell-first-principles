{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy as LBS (ByteString)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

sectionJson :: LBS.ByteString
sectionJson = [r|
{
    "section": {"host":"wikipedia.org"},
    "whatisit": {"red":"intoothandclaw"}
}
|]

data TestData = 
    TestData {
        section :: Host,
        what :: Color
    } deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

data Color = Red String | Blue String | Yellow String deriving (Eq, Show)

instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section" <*> v .: "whatisit"
    parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where
    parseJSON (Object v) =
        Host <$> v .: "host"
    parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
    parseJSON (Object v) = (Red <$> v .: "red") <|> (Blue <$> v .: "blue") <|> (Yellow <$> v .: "yellow")
    parseJSON _ = fail "Expected an object for Color"

data NumberOrString = Numba Integer | Stringy Text deriving (Eq, Show)

instance FromJSON NumberOrString where
    parseJSON (Number i) =
        case floatingOrInteger i of
            (Left _) -> fail "Must be integral number"
            (Right integer) -> return $ Numba integer
    parseJSON (String s) = return $ Stringy s
    parseJSON _ = fail "NumberOrString must be number or string"

dec :: LBS.ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString -> Either String NumberOrString
eitherDec = eitherDecode

test = do
    let blah :: Maybe Value
        blah = decode sectionJson
    -- print blah
    print $ dec "blah"
    print $ eitherDec "blah"