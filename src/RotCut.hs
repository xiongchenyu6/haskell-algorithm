{-# LANGUAGE OverloadedStrings #-}

module RotCut where

import           Control.Monad

test = "hat"

permutation :: String -> [String]
permutation = foldr ((=<<) . insert') [[]]


insert' :: Char -> String -> [String]
insert' c []         = [[c]]
insert' c l@(x : xs) = (c : l) : ((x :) <$> insert' c xs)

main = forM_ (permutation test) putStrLn
