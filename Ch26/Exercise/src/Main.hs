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
import System.Random
import Control.Monad.Except

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

playAGame x = do
  f <- (/0.5) . (subtract 3.9343) . (*5) . (+52.8)
  g <- (*10)
  return (f - g)

----------------------------------------------------------------

random3'' = let (r1, g1) = random (mkStdGen 7); (r2, g2) = random g1; (r3, g3) = random g2 in [(r1, g1), (r2, g2), (r3, g3)] :: [(Bool, StdGen)]

random3 i = collectNext $ collectNext $ [random $ mkStdGen i]
  where collectNext xs@((i, g):_) = [random g] ++ xs

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

----------------------------------------------------------------

getString :: ExceptT String IO String
getString = do {
  line <- liftIO getLine;
  if (null line) then
    throwError "empty input"
  else
    return line
} `catchError` (\_ -> return "Error occurred, use default string")

safeIO = do
  Right line <- runExceptT getString
  putStrLn line

----------------------------------------------------------------

type Except e = ExceptT e Identity

except :: Either e a -> Except e a
except m = ExceptT (Identity m)

runExcept :: Except e a -> Either e a
runExcept (ExceptT m) = runIdentity m

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------

----------------------------------------------------------------