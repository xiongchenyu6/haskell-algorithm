import Data.Array as A
import Data.Set as S
import Data.List as L

target :: Array (Int,Int) Int
target = array ((0,0), (3, 3)) [((0,0),0),
                                ((0,1),0),
                                ((0,2),0),
                                ((0,3),0),
                                ((1,0),0),
                                ((1,1),0),
                                ((1,2),0),
                                ((1,3),0),
                                ((2,0),0),
                                ((2,1),0),
                                ((2,2),1),
                                ((2,3),0),
                                ((3,0),0),
                                ((3,1),0),
                                ((3,2),0),
                                ((3,3),0)]

main = do
    let final = array ((0,0), (3, 3)) [((i, j), 0) | i <- [0..3], j <- [0..3]]
    let n = move target S.empty ([final],0)
    putStrLn $ (show n)



compareA :: Array (Int,Int) Int -> Array (Int,Int) Int -> Bool
compareA a1 a2 = a1 == a2


move :: Array (Int,Int) Int -> Set (Array (Int,Int) Int) -> ([Array (Int,Int) Int] ,Int) -> Int
move target set @acc(l,sum)
  | any (compareA target) l = sum
  | otherwise = move target (i nL) (nL ,(sum +1))
  where
    newList = getAllNextMove <$> l >>= id
    i array= Prelude.foldl (\acc v -> S.insert v acc) set $ array
    f::[Array (Int,Int) Int] -> [Array (Int,Int) Int]
    f array= L.filter (\x -> not (member x set)) array
    nL = f newList


getAllNextMove :: Array (Int,Int) Int ->  [Array (Int,Int) Int]
getAllNextMove a = [(click i j a)| i<- [0..3] , j<-[0..3]]

click :: Int -> Int -> Array (Int,Int) Int  -> Array (Int,Int) Int
click y x a =
  let l1 = [(y,i) | i<- [0..3]]
      l2 = [(i,x) | i<- [0..3]]
  in change l1 (change l2 a)

  where
    change [] a = a
    change (x:xs) a = change xs a // [(x, toggle $ a ! x)]

toggle :: Int -> Int
toggle 1 = 0
toggle 0 = 1
