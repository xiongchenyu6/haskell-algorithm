#!/usr/bin/env stack
-- stack script --resolver lts-12.16

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.List
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad

evil :: IO ()
evil = forever $ do
  eres <- try $ threadDelay 1000000
  print (eres :: Either SomeException ())

main :: IO ()
main = withAsync evil $ const $ return ()

getResult1 :: IO Int
getResult1 = mask_ $ do
  putStrLn "Doing some big computation..."
  threadDelay 20
  putStrLn "Done!"
  error "fuck"

getResult2 :: IO Int
getResult2 = mask_ $ do
  putStrLn "Doing some smaller computation..."
  threadDelay 1000000
  putStrLn "Done!"
  return 41
  error "fuck2"

aaaa = mask $ \restore -> do
    x <- getLine
    restore (readFile x) `onException` putStrLn "bbb"
    putStrLn "fff"

bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r


waitAnyNoExpCancel :: [Async a] -> IO (Async a, a)
waitAnyNoExpCancel asyncs@(_:_:_) = do
  (t, res) <- waitAnyCatch asyncs
  case res of
      Left _  -> waitAnyNoExpCancel (delete t asyncs)
      Right r -> mapM cancel asyncs >> return (t, r)
waitAnyNoExpCancel asyncs       = waitAny asyncs
