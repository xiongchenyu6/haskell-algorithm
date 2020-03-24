{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module RandomDie where
import           Control.Applicative            ( liftA3 )
import           Control.Monad                  ( replicateM )
import           Control.Monad.State
import           System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  -- Use this tactic _extremely_ sparingly.
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x -> let (m, n) = g x in (f m, n)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a, )

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \x ->
    let (a  , s ) = g x
        (af', s') = f s
    in  (af' a, s')

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \x -> let (m, n) = f x in runMoi (g m) n


a = runState (modify (+ 8) >> modify (* 10)) 10

fib' :: Int -> State Int Int
fib' 1 = modify (+ 1) >> return 0
fib' 2 = modify (+ 1) >> return 1
fib' n = modify (+ 1) >> (+) <$> fib' (n - 1) <*> fib' (n - 2)
