module MonadBase where

import Control.Applicative
import Control.Monad.State

main = do
  let f = do
           modify (+ 1)
           show <$> get :: StateT Int IO String
  flip runStateT 0 $ do
    x <- f
    y <- get
    y' <-
      liftIO $ do
        print $ "x = " ++ show x -- x = "1"
        (x', y') <- runStateT f y
        print $ "x = " ++ show x' -- x = "2"
        return y'
    put y'
