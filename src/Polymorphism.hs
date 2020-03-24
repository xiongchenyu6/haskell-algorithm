module Polymorphism where
import Data.List

data Item  =Singular Int | Multi (Int,Int)

instance Show Item where
    show (Singular x) = show x
    show (Multi (x,y))= show (x,y)

encodeList :: [Int]
encodeList = [1,1,2,3,3,3,3,4,4,5,6,6,6]

itemList :: [Item]
itemList = foldr (\x l -> if length x == 1 then Singular (head x) : l else Multi (head x,length x):l ) [] . group $ encodeList
