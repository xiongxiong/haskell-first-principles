{-# LANGUAGE OverloadedStrings #-}

module ExBS where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

s :: String
s = "\12371\12435\12395\12385\12399\12289\20803\27671\12391\12377\12363\65311"

utf8ThenPrint :: B.ByteString -> IO ()
utf8ThenPrint = putStrLn . T.unpack . TE.decodeUtf8

throwsException :: IO ()
throwsException = utf8ThenPrint (B8.pack s)

bytesByWayOfText :: B.ByteString
bytesByWayOfText = TE.encodeUtf8 (T.pack s)

libraryDoesTheWork :: B.ByteString
libraryDoesTheWork = UTF8.fromString s

thisWorks :: IO ()
thisWorks = utf8ThenPrint bytesByWayOfText

alsoWorks :: IO ()
alsoWorks = utf8ThenPrint libraryDoesTheWork

main :: IO ()
main = do
    TIO.putStrLn $ TE.decodeUtf8 (s input)
    TIO.putStrLn $ TE.decodeUtf8 (s compressed)
    where s = BL.toStrict