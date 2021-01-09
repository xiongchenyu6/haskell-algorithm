{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances, DeriveFunctor, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, RankNTypes, GADTs, DefaultSignatures #-}

module DifferentiableFunctor where

import           Control.Applicative

class Functor f => Comonad f where
  extract   :: f a -> a
  extend    :: (f a -> b) -> (f a -> f b)
  extend f = fmap f . duplicate
  duplicate :: f a -> f (f a)
  duplicate = extend id

class (Functor t, Functor (D t)) => Diff t where
  type D t :: * -> *
  up       :: Zip t a -> t a
  down     :: t a -> t (Zip t a)
  around   :: Zip t a -> Zip t (Zip t a)
  d2       :: proxy (t a) -> Dict (Diff (D t))

  around z@(d :<-: h) = case d2 (Nothing :: Maybe (t ())) of
    Dict -> fmap (\z' -> up z' :<-: extract z') (down d) :<-: z

data Dict c where
  Dict ::c => Dict c

data Zip t a = D t a :<-: a
deriving instance (Eq (D t a),   Eq a)   => Eq   (Zip t a)
deriving instance (Show (D t a), Show a) => Show (Zip t a)

cx :: Zip t a -> D t a
cx (cx :<-: _) = cx

overz :: (D f a -> D g a) -> Zip f a -> Zip g a
overz f (cx :<-: a) = f cx :<-: a

instance Functor (D t) => Functor (Zip t) where
  fmap f (cx :<-: a) = fmap f cx :<-: f a

instance Diff t => Comonad (Zip t) where
  extract (_ :<-: a) = a
  duplicate = around

newtype Void  = Void { absurd :: forall a . a }
newtype K a x = K a deriving ( Eq, Show, Functor )
newtype I x   = I x deriving ( Eq, Show, Functor )

infixl 3 :*
infixl 8 &
data    (+) f g x = Inl (f x) | Inr (g x) deriving ( Eq, Show, Functor )
data    (:*:) f g x = f x :* g x            deriving ( Eq, Show, Functor )
newtype (&) f g x = C (f (g x))           deriving ( Eq, Show, Functor )

--------------------------------------------------------------------------------

instance Diff (K a) where
  type D (K a) = K Void
  up (K v :<-: m) = absurd v
  down (K a) = K a
  around (K v :<-: z) = absurd v
  d2 _ = Dict

instance Diff I where
  type D I = K ()
  up (_ :<-: a) = I a
  down (I a) = I (K () :<-: a)
  around z@(K () :<-: x) = K () :<-: z

zup :: Diff t => D t a -> a -> t a
zup df a = up (df :<-: a)

zaround :: Diff t => D t a -> a -> D t (Zip t a)
zaround df a = cx (around (df :<-: a))

instance (Diff f, Diff g) => Diff (f + g) where
  type D (f + g) = D f + D g -- linearity!
  up (p :<-: a) = case p of
    Inl df -> Inl (up (df :<-: a))
    Inr dg -> Inr (up (dg :<-: a))
  down p = case p of
    Inl f -> Inl (overz Inl <$> down f)
    Inr g -> Inr (overz Inr <$> down g)
  -- around z@(p :<-: (a :: a)) = case p of
  --   Inl df -> Inl (overz Inl <$> (zaround df a :: D f (Zip f a))) :<-: z
  --   Inr dg -> Inr (overz Inr <$> (zaround dg a :: D g (Zip g a))) :<-: z

  d2 _ = case (d2 (Nothing :: Maybe (f ())), d2 (Nothing :: Maybe (g ()))) of
    (Dict, Dict) -> Dict

instance (Diff f, Diff g) => Diff (f :*: g) where
  type D (f :*: g) = (D f :*: g) + (f :*: D g)
  up (p :<-: a) = case p of
    Inl (df :* g ) -> up (df :<-: a) :* g
    Inr (f  :* dg) -> f :* up (dg :<-: a)
  down (f :* g) =
    overz (Inl . (:* g)) <$> down f :* overz (Inr . (f :*)) <$> down g
  -- around z@(p :<-: (a :: a)) = cx' :<-: z where
  --   cx' = case p of
  --     Inl (df :* g) ->
  --       Inl $  overz (Inl . (:* g)       ) <$> (zaround df a :: D f (Zip f a))
  --           :* overz (Inr . (zup df a :*)) <$> down g
  --     Inr (f :* dg) ->
  --       Inr $  overz (Inl . (:* zup dg a)) <$> down f
  --           :* overz (Inr . (f :*)       ) <$> (zaround dg a :: D g (Zip g a))

  d2 _ = case (d2 (Nothing :: Maybe (f ())), d2 (Nothing :: Maybe (g ()))) of
    (Dict, Dict) -> Dict

