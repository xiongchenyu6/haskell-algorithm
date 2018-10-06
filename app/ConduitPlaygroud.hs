{-# LANGUAGE ExistentialQuantification #-}
module ConduitPlaygroud where

import           Conduit
import           Control.Monad
import           Data.Conduit.List
import           System.IO

loudYield :: forall i. Int -> ConduitM i Int IO ()
loudYield x = do
  liftIO $ putStrLn $ "yielding: " ++ show x
  yield x
loudSinkNull :: forall o. ConduitM Int o IO ()
loudSinkNull =
  mapM_C $ \x -> putStrLn $ "awaited: " ++ show x
main :: IO ()
main =
  runConduit $ Control.Monad.mapM_ loudYield [1..3]
            .| loudSinkNull

leftOver = print
     $ runConduitPure
     $ yieldMany [1 .. 10 :: Int]
    .| ((,)
            <$> (takeWhileC (< 6) .| sinkList)
            <*> sinkList)

-- sumC will comsume list already
bug = print
     $ runConduitPure
     $ yieldMany [1..10 :: Double]
    .| ((/)
            <$> sumC
            <*> (fromIntegral <$> lengthC))


stdinLines :: ConduitT () String IO ()
stdinLines = do
    eof <- lift isEOF
    unless eof $ do
        line <- lift getLine
        yield line
        stdinLines

stdoutLines :: ConduitT String Void IO ()
stdoutLines = do
    ml <- await
    case ml of
        Nothing -> return () -- upstream source finished
        Just l  -> lift (putStrLn l) >> stdoutLines

main1 =  runConduit $ stdinLines .| Data.Conduit.List.map (show . length) .| stdoutLines
