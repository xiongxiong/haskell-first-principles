{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State hiding (get)
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import Control.Monad
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
import Criterion.Main

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
bumpBoomp k m = 
  let v = M.lookup k m in
    case v of
      Nothing -> (M.insert k 1 m, 1)
      Just v' -> (M.insert k (v' + 1) m, v' + 1)

app :: Scotty () 
app = get "/:key" $ do
  unprefixed <- param "key" :: Handler String
  let m = undefined :: M.Map Text Integer
  let key' = TL.pack $ mappend undefined unprefixed
  let newInteger = snd $ bumpBoomp key' m
  html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

testApp :: IO ()
testApp = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config {count = counter, prefix = TL.pack prefixArg}
      runR rma = runReaderT rma config
  scottyT 3000 runR app

----------------------------------------------------------------

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList = DiffList . (++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty = DiffList ([]++)
  mappend = (<>)

countdown n = if (n == 0) then do {tell ["0"]} else do {countdown (n - 1); tell [show n]} :: Writer [String] ()

countdownTest :: Int -> [String]
countdownTest = snd . runWriter . countdown

countdown' n = if (n == 0) then do {tell $ toDiffList ["0"]} else do {countdown' (n - 1); tell $ toDiffList [show n]} :: Writer (DiffList String) ()

countdownTest'  :: Int -> [String]
countdownTest' = fromDiffList . snd . runWriter . countdown'

benchTest :: Int -> IO ()
benchTest n = do
  defaultMain 
    ([
      bench "countdown using ++" . nf countdownTest,
      bench "countdown using DiffList" . nf countdownTest'
    ] <*> pure n)

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------