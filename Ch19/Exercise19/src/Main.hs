{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Web.Scotty
import Database.Persist.Sql
import Database.Persist.Postgresql
import Conduit
import Data.Conduit.Combinators
-- import XMonad
-- import XMonad.Actions.Volume
-- import Data.Map.Lazy (fromList)
import Data.Monoid (mappend)
import Functor

main :: IO ()
main = scotty 3000 $
  Web.Scotty.get "/:word" $ do
    beam <- param "word"
    html $ mconcat [
        "<h1>Scotty, ",
        beam,
        " me up!</h1>"
      ]

-- runDb :: SqlPersist (ResourceT IO) a -> IO a
-- runDb query = do
--   let connStr = foldr (\(k,v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
--   runResourceT . withPostgresqlConn connStr $ runSqlConn query

-- runX = do
--   xmonad def {
--     keys =
--       \c -> fromList [
--         ((0, xK_F6), lowerVolume 4 >> return ()),
--         ((0, xK_F7), raiseVolume 4 >> return ())
--       ] `mappend` keys defaultConfig c
--   }