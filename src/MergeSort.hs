module MergeSort where
unsortList = [6, 5, 2, 3, 8, 1]

mergeSort :: [Int] -> [Int]
mergeSort l
  | (length l) > 1 = merge (mergeSort (frontHalf $ l))
                           (mergeSort (backHalf $ l))
                           []
  | otherwise = l
 where
  frontHalf = flip take <*> half
  backHalf  = flip drop <*> half
  half      = flip div 2 . length


-- merge 2 sorted list
merge :: [Int] -> [Int] -> [Int] -> [Int]
merge []       []       ans = ans
merge []       (x : xs) ans = merge [] xs (ans ++ [x])
merge (x : xs) []       ans = merge xs [] (ans ++ [x])
merge xt@(x : xs) yt@(y : ys) ans | x <= y    = merge xs yt (ans ++ [x])
                                  | otherwise = merge xt ys (ans ++ [y])


quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort l@(x : xs)
  | (length l) == 1 = l
  | otherwise       = (quickSort leftList) ++ [x] ++ (quickSort rightList)
 where
  leftList  = filter (< x) xs
  rightList = filter (>= x) xs
