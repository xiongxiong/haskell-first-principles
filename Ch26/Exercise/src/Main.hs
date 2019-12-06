{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (get)
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

main :: IO ()
main = do
  putStrLn "hello world"

----------------------------------------------------------------

rDec :: Num a => Reader a a
-- rDec = ReaderT $ \r -> pure (r - 1)
rDec = ReaderT $ (<*>) (pure (subtract 1)) pure

----------------------------------------------------------------

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ pure . show

----------------------------------------------------------------

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
-- rPrintAndInc = ReaderT $ \r -> do
--   putStrLn $ "Hi: " ++ show r
--   return $ r + 1
rPrintAndInc = ReaderT $ (>>) <$> (putStrLn . (++) "Hi: " . show) <*> (pure . (+1))

----------------------------------------------------------------

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
-- sPrintIncAccum = StateT $ \s -> do
--   putStrLn $ "Hi: " ++ show s
--   return (show s, s + 1)
sPrintIncAccum = StateT $ (>>) <$> (putStrLn . (++) "Hi: " . show) <*> (pure . ((,) <$> show <*> (+1)))

----------------------------------------------------------------

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  -- v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

----------------------------------------------------------------

data Config = Config {
    count :: IORef (M.Map Text Integer),
    prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = undefined 

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  let key' = mappend undefined unprefixed
  newInteger <- undefined 
  html $ mconcat ["<h1> Success! Count was: ", TL.pack $ show newInteger, "</h1>"] 

appTest :: IO ()
appTest = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = undefined
      runR = undefined
  scottyT 3000 runR app

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------