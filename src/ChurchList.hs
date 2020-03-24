{-# LANGUAGE RankNTypes #-}

module ChurchList where

import Prelude
import Control.Applicative
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

-- | Laws:
--
-- > runList xs cons nil == xs
-- > runList (fromList xs) f z == foldr f z xs
-- > foldr f z (toList xs) == runList xs f z
newtype ChurchList a =
    ChurchList { runList :: forall r. (a -> r -> r) -> r -> r }

-- | Make a 'ChurchList' out of a regular list.
fromList :: [a] -> ChurchList a
fromList xs = ChurchList $ \k z -> foldr k z xs

-- | Turn a 'ChurchList' into a regular list.
toList :: ChurchList a -> [a]
toList xs = runList xs (:) []

-- | We can construct an empty 'ChurchList' without using a @[]@.
nil :: ChurchList a
nil = ChurchList $ \_ z -> z

-- | The 'ChurchList' counterpart to '(:)'.  Unlike 'DList', whose
-- implementation uses the regular list type, 'ChurchList' doesn't
-- rely on it at all.
cons :: a -> ChurchList a -> ChurchList a
cons x xs = ChurchList $ \k z -> k x (runList xs k z)
-- \k xz -> k x (z)
-- | Append two 'ChurchList's.  This runs in O(1) time.  Note that
-- there is no need to materialize the lists as @[a]@.
append :: ChurchList a -> ChurchList a -> ChurchList a
append xs ys = ChurchList $ \k z -> runList xs k (runList ys k z)

-- | Map over a 'ChurchList'.  No need to materialize the list.
instance Functor ChurchList where
    fmap f xs = ChurchList $ \k z -> runList xs (\x xs' -> k (f x) xs') z

-- | The 'Foldable' instance is trivial, given the 'ChurchList' law.
instance Foldable ChurchList where
    foldr f z xs = runList xs f z

instance Traversable ChurchList where
    traverse f xs = runList xs step (pure nil)
        where step x rest = cons <$> f x <*> rest