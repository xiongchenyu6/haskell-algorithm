import           Data.Array.IArray as A
import           Data.List         as L
import           Data.Map          as M
import           System.IO

type Status = (Int,Int)
type Point = (Int,Int)
type Route = (Status,[Point])

main = do
    contents <- readFile "/Users/xiongchenyu/github/haskell-algorithm/src/map.txt"
    let array = toArray (tail ((fmap.fmap) (read::String -> Int) (words <$> (lines contents))))
    let tops = findInDegreeZero 0 0 array []
    print (findBestRoute tops array)

toArray :: [[Int]] -> Array Int (Array Int Int)
toArray l = listArray (0,999) (listArray (0,999) <$>l)

-- row col
findInDegreeZero :: Int -> Int -> Array Int (Array Int Int) -> [(Int,Int)] -> [(Int,Int)]
findInDegreeZero y@999 x@999 array acc = if testInDegree y x array then
                                  (y,x) : acc
                                else
                                  acc
findInDegreeZero y x@999 array acc = if testInDegree y x array then
                              findInDegreeZero (y+1) 0 array ((y,x) : acc)
                            else
                              findInDegreeZero (y+1) 0 array acc
findInDegreeZero y x array acc = if testInDegree y x array then
                          findInDegreeZero y (x+1) array ((y,x): acc)
                        else
                          findInDegreeZero y (x+1) array acc

testInDegree :: Int -> Int -> Array Int (Array Int Int) -> Bool
testInDegree y x array = all (current >) [top,button,left,right]
  where
    current = array A.! y A.! x
    top =
      if y == 0 then
        0
      else
        array A.! (y -1) A.! x
    button =
      if y == 999 then
        0
      else
        array A.! (y + 1) A.! x
    left =
      if x == 0 then
        0
      else
        array A.! y A.! (x -1 )
    right =
      if x == 999 then
        0
      else
        array A.! y A.! (x+1)

findBestRoute :: [Point] -> Array Int (Array Int Int) -> Route
findBestRoute l a = fst (L.foldl (\acc v -> compareR acc (find acc v)) (intRoute,intMap) l)
  where
    find acc v = findPath v a (intRoute,snd acc)
    compareR a b = if fst (fst a)> fst (fst b) then
                    a
                  else
                    b
    intRoute = ((0,0),[])
    intMap :: Map Point Route
    intMap = M.empty

-- Use Map for dynamic programming to cache the path
findPath :: Point -> Array Int (Array Int Int) -> (Route,Map Point Route) -> (Route, Map Point Route)
findPath (y,x) array acc@(((l,w),r),m) = maximum [top,button,left,right]
  where
    current = array A.! y A.! x
    tV  = array A.! (y-1) A.! x
    bV = array A.! (y +1 ) A.! x
    lV = array A.! y A.! (x-1)
    rV = array A.! y A.! (x +1)
    findAndMove a b v = case m M.!? (a,b) of
      Just((l',w'),r') -> (((l+l',w+w'-(current -v)),r++r'),m)
      Nothing -> findPath (a,b) array (((l+1,w +(current -v)),r ++ [(y,x)]), updatedMap)
    updatedMap= M.insert (y,x) ((l,w),r++[(y,x)]) m
    edge = (((l,w),r ++[(y,x)]),m)
    top = if y == 0 || tV >= current then
            edge
          else
            findAndMove (y-1) x tV

    button = if y == 999 || bV>= current then
               edge
             else
               findAndMove (y+1) x bV
    left = if x == 0 || lV >= current then
             edge
           else
               findAndMove y (x-1) lV
    right = if x == 999 || rV >= current then
              edge
            else
               findAndMove y (x+1) rV
