module CellularAutomate where

import           Text.ParserCombinators.Parsec

data Tree = Node { left::Tree, mid::Char, right::Tree } | Nil


value :: Parser Char
value = oneOf "X."


