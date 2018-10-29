{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Kind
import qualified Prelude as P(read, (+), Show, show)

data Z deriving P.Show
data S n deriving P.Show

data Nat a where
    Zero :: Nat Z
    Succ :: Nat a -> Nat (S a)

instance P.Show (Nat a) where
  show Zero = "0"
  show (Succ a) = P.show ( 1 P.+ (P.read (P.show a)))

type family (:+:) (n :: *) (m :: *) :: *
type instance Z   :+: m = m
type instance S n :+: m = S (n :+: m)

type family (:*:) (n :: *) (m :: *) :: *
type instance Z   :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

type family (:-:) (n :: *) (m :: *) :: *
type instance n :-: Z = n
type instance Z :-: m = Z
type instance S n :-: S m = n :-: m

type family (:^:) (n :: *) (m :: *) :: *
type instance Z :^: n = n
type instance n :^: Z = n
type instance S n :^: S m = S (n :^: m)

type family (:~:) (n :: *) (m :: *) :: *
type instance Z :~: m = Z
type instance n :~: Z = Z
type instance S n :~: S m = S (n :~: m)

infixl 4 +
(+) :: Nat n -> Nat m -> Nat (n :+: m)
Zero + a = a
(Succ x) + a = Succ (x Main.+ a)

infixl 5 *
(*) :: Nat n -> Nat m -> Nat (n :*: m)
Zero * _ = Zero
(Succ x) * m = m Main.+ x Main.* m

data Vec a n where
    VNil :: Vec a Z
    VCons :: a -> Vec a n -> Vec a (S n)

(++) :: Vec a n -> Vec a m -> Vec a (n :+: m)
VNil ++ ys = ys
VCons x xs ++ y = VCons x (xs Main.++ y)

repeat :: Nat n -> Vec a m -> Vec a (n :*: m)
repeat Zero _ = VNil
repeat (Succ x) xs = xs Main.++ Main.repeat x xs


headV :: Vec a (S n) -> a
headV (VCons x _) = x

tailV :: Vec a (S n) -> Vec a n
tailV (VCons _ xs) = xs

zip :: Vec a n -> Vec b m -> Vec (a, b) (n :~: m)
zip VNil _ = VNil
zip _ VNil = VNil
zip (VCons x xs) (VCons y ys) = VCons (x, y) (Main.zip xs ys)
