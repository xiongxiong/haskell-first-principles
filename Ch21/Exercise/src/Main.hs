module Main where

import Data.Traversable

main :: IO ()
main = do
  putStrLn "hello world"

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn query = do
--   a <- fetchFn query
--   case sequence (map decodeFn a) of
--     (Left err) -> return $ Left $ err
--     (Right res) -> do
--       a <- makeIoOnlyObj res
--       return $ Right a
-- pipelineFn query = do
--   a <- fetchFn query
--   traverse makeIoOnlyObj (mapM decodeFn a)
-- pipelineFn = 
--   (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn
pipelineFn =
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn 