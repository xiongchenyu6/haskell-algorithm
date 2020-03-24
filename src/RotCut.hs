module RotCut where
test = "ab"

permutation :: String -> [String]
permutation []       = [[]]
permutation (x : xs) = insert' x =<< (permutation xs)

insert' :: Char -> String -> [String]
insert' c []         = [[c]]
insert' c l@(x : xs) = (c : l) : ((x :) <$> (insert' c xs))
