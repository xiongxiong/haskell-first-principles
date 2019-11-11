{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Web.Scotty (scotty, get, html)
import Database.Persist.Sql hiding (get)
import Data.Conduit
import Data.Conduit.Lift

main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat [
        "<h1>Scotty, ",
        beam,
        " me up!</h1>"
      ]

runDb :: SqlPersist (ResourceT IO) a -> IO a
runDb query = do
  let connStr = foldr (\(k,v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
  runResourceT . withPostgresqlConn connStr $ runSqlConn query