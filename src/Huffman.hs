module Huffman where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Function

class Eq a => Bits a where
  zer :: a
  one :: a

instance Bits Int where
  zer = 0
  one = 1

instance Bits Bool where
  zer = False
  one = True

type Codemap a = M.Map Char [a]

data HTree = Leaf Char Int
           | Fork HTree HTree Int
           deriving(Show)

weight::HTree -> Int
weight (Leaf _ w)   = w
weight (Fork _ _ w) = w

merge :: HTree -> HTree -> HTree
merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

stringToTree :: String -> HTree
stringToTree = buildTree . freqList
  where freqList :: String -> [(Char, Int)]
        freqList = M.toList . M.fromListWith (+) . map (flip (,) 1)

        buildTree :: [(Char, Int)] ->HTree
        buildTree = bld . map (uncurry Leaf) . sortBy (on compare snd)
                where bld (t:[]) = t
                      bld (a:b:cs) = bld $ insertBy (on compare weight) (merge a b) cs

buildCodemap :: Bits a => HTree -> Codemap a
buildCodemap = M.fromList . buildCodelist
         where buildCodelist (Leaf c w) = [(c,[])]
               buildCodelist (Fork l r w) = map (addBit zer) (buildCodelist l) ++ map (addBit one) (buildCodelist r)
               addBit b = second (b :)

encode :: Bits a => Codemap a -> String -> [a]
encode m = concat . map (m M.!)

decode :: Bits a => HTree -> [a] -> String
decode tree = dcd tree
       where dcd (Leaf c _) []    =    [c]
             dcd (Leaf c _) bs    =     c : dcd tree bs
             dcd (Fork l r _) (b:bs) = dcd (if b == zer then l else r) bs
