test = "ab"

permutation :: String -> [String]
permutation [] = [[]]
permutation (x:xs) = (permutation xs) >>= insert' x

insert' :: Char -> String -> [String]
insert' c [] = [[c]]
insert' c l@(x:xs) = (c : l) : (map (x:) (insert' c xs))
