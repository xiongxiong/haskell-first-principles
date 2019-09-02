module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar _ [] = ""
caesar a (x:xs) = (chr $ mod (ord x - 97 + a) 26 + 97) : caesar a xs

unCaesar :: Int -> String -> String
unCaesar _ [] = ""
unCaesar a (x:xs) = (chr $ mod (ord x - 97 - a) 26 + 97) : unCaesar a xs

