import Control.Concurrent
import Control.Monad
main = do
  putStrLn "please input array1:" -- stand for not no found
  array1 <- fmap (read::String->[Double]) getLine
  putStrLn "please input array2" -- stand for not no found
  array2 <- fmap (read::String->[Double]) getLine
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  putMVar m1 (head  array1)
  forkIO $ do
    threaTask m1 m2 array2 array2
  threaTask m2 m1 (tail array1) array1
  where
    convert = map (read::String -> Double)
    threaTask _ m2 [] _ = do
         putStrLn "无" -- stand for not no found
         putMVar m2 (0.5)
    threaTask m1 m2 array1 array2 = do
      v <- takeMVar m1
      if elem v array2 then
          putMVar m2 (0.5) >>
          putStrLn "有" -- stand for exist
      else
          if v == 0.5 then
            putStrLn "停" -- stand for stop
          else
            do
              putMVar m2 (head array1)
              threaTask m1 m2 (tail array1) array2

data Thread m r = Atomic (m (Thread m r)) | Return r

atomic :: (Monad m) => m a -> Thread m a
atomic m = Atomic $ (liftM Return) m

instance (Functor m) => Functor (Thread m) where
  fmap f (Return x) = Return (f x)

instance (Applicative m) => Applicative (Thread m) where
  pure = Return
  (<*>) = undefined

instance (Monad m) => Monad (Thread m) where
    return = Return
    (Atomic m) >>= f = Atomic (liftM (>>= f) m)
    (Return r) >>= f = f r

thread1 :: Thread IO ()
thread1 = (atomic $ print 1) >> (atomic $ print 2)

thread2 :: Thread IO ()
thread2 = do
    str <- atomic $ getLine
    atomic $ putStrLn str

interleave ::
    (Monad m) => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) =
    atomic m1 >>= \x ->
    atomic m2 >>= \y ->
    interleave x y
interleave t1 (Return _) = t1
interleave (Return _) t2 = t2

runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r


interleave' ::
    (Monad m) => Thread m r -> Thread m r -> Thread m r
interleave' a@(Atomic m1) b@(Atomic m2) =
    a >>= \x ->
    b >>= \y ->
    interleave' x y
interleave' t1 (Return _) = t1
interleave' (Return _) t2 = t2
