module Redmart1 where

import           Data.Array.IArray             as A
import           Data.List                     as L
import           Data.Map                      as M
import           System.IO

type Status = (Int, Int)
type Point = (Int, Int)
type PathWithMap = (Status, Map Point Status)

main :: IO ()
main = do
  contents <- readFile "/Users/xiongchenyu/github/haskell-algorithm/src/map.txt"
  let list = (fmap . fmap) (read :: String -> Int) (words <$> lines contents)
  let array = toArray (head . head $ list) (tail list)
  let tops  = findInDegreeZero array
  print (findBestRoute tops array)

toArray :: Int -> [[Int]] -> Array Int (Array Int Int)
toArray n l = listArray (0, n - 1) $ listArray (0, n - 1) <$> l

-- row col
findInDegreeZero :: Array Int (Array Int Int) -> [Point]
findInDegreeZero array = findInDegreeZero' 0 0 array []
 where
  findInDegreeZero'
    :: Int -> Int -> Array Int (Array Int Int) -> [Point] -> [Point]
  findInDegreeZero' y@999 x@999 array acc = selectIndegreeZero y x array acc
  findInDegreeZero' y x@999 array acc =
    findInDegreeZero' (y + 1) 0 array $ selectIndegreeZero y x array acc
  findInDegreeZero' y x array acc =
    findInDegreeZero' y (x + 1) array $ selectIndegreeZero y x array acc


selectIndegreeZero
  :: Int -> Int -> Array Int (Array Int Int) -> ([Point] -> [Point])
selectIndegreeZero y x array = if all (current >) [top, button, left, right]
  then ((y, x) :)
  else id
 where
  current = array A.! y A.! x
  top     = if y == 0 then 0 else array A.! (y - 1) A.! x
  button  = if y == 999 then 0 else array A.! (y + 1) A.! x
  left    = if x == 0 then 0 else array A.! y A.! (x - 1)
  right   = if x == 999 then 0 else array A.! y A.! (x + 1)

findBestRoute :: [Point] -> Array Int (Array Int Int) -> Status
findBestRoute l a = fst
  (L.foldl (\acc v -> comparePathMap acc (find acc v)) (intRoute, intMap) l)
 where
  find acc v = findPath v a (intRoute, snd acc)
  intRoute = (1, 0)
  intMap :: Map Point Status
  intMap = M.empty

-- Use Map for dynamic programming to cache the path
findPath :: Point -> Array Int (Array Int Int) -> PathWithMap -> PathWithMap
findPath p@(y, x) array acc@((l, w), m) = L.foldl comparePathMap
                                                  top
                                                  [button, left, right]
 where
  current = array A.! y A.! x
  tV      = array A.! (y - 1) A.! x
  bV      = array A.! (y + 1) A.! x
  lV      = array A.! y A.! (x - 1)
  rV      = array A.! y A.! (x + 1)
  findAndMove a b v = case m M.!? (a, b) of
    Just (l', w') -> ((l + l' - 1, w + w' - (current - v)), m) -- delete one overlaping vertex
    Nothing -> findPath (a, b) array ((l + 1, w + (current - v)), updatedMap)
  updatedMap = M.insert p (l, w) m
  top = if y == 0 || tV >= current then acc else findAndMove (y - 1) x tV
  button = if y == 999 || bV >= current then acc else findAndMove (y + 1) x bV
  left = if x == 0 || lV >= current then acc else findAndMove y (x - 1) lV
  right = if x == 999 || rV >= current then acc else findAndMove y (x + 1) rV

comparePathMap :: PathWithMap -> PathWithMap -> PathWithMap
comparePathMap a b = if fst a > fst b then a else b
