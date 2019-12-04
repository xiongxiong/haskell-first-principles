{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid (mconcat)

main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        let hello = putStrLn "hello"
        -- (lift :: IO a -> ActionM a) hello
        -- (ActionT . lift . lift . lift) (putStrLn "hello")
        liftIO (putStrLn "hello")
        html $ 
            mconcat ["<h1>Scotty, ",
                beam,
                " me up!</h1>"]