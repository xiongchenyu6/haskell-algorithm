{-# LANGUAGE TemplateHaskell #-}

import MySplices
import Control.Monad (forM)
import ConstSplices
import Language.Haskell.TH

two :: Int
two = $(pure myExp) + $(pure myExp)
-- n = 1
f :: (Int, Int) -> String
f $(pure myPat) = "1 and 2"
f _             = "something else"
mint :: $(pure myType)
mint = Just two

n= 5
main = print $(pure $ VarE $ mkName "n")
  where n = 1

$(forM [1..15] constN)
