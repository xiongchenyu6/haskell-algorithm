-- # pragmas
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Hlist where
import           Data.Kind                      ( Constraint
                                                , Type
                                                )


data HList (ts :: [Type]) where
  HNil ::HList '[]
  (:#) ::t->HList ts->HList(t':ts)

infixr 5 :#
