module Main where

import           Data.List                      ( break )

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

x = 10

main :: IO ()
main = putStrLn "hello world"

run :: IO (Maybe FSZipper)
run = return $ fsTo "pics" (myDisk, []) >>= fsTo "watermelon_smash.gif"

myDrop :: (Ord t, Num t) => t -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs then xs else myDrop (n - 1) (tail xs)

data List a = Cons a (List a) | Nil deriving (Show)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z where step x g a = g (f a x)

fromList :: List a -> [a]
fromList (Cons x xs) = x : fromList xs
fromList Nil         = []

myDisk :: FSItem
myDisk = Folder
  "root"
  [ File "goat_yelling_like_man.wmv" "baaaaaa"
  , File "pope_time.avi"             "god bless"
  , Folder
    "pics"
    [ File "ape_throwing_up.jpg"  "bleargh"
    , File "watermelon_smash.gif" "smash!!"
    , File "skull_man(scary).bmp" "Yikes!"
    ]
  , File "dijon_poupon.doc" "best mustard"
  , Folder
    "programs"
    [ File "fartwizard.exe"  "10gotofart"
    , File "owl_bandit.dmg"  "mov eax, h00t"
    , File "not_a_virus.exe" "really not a virus"
    , Folder
      "source code"
      [ File "best_hs_prog.hs" "main = print (fix error)"
      , File "random.hs"       "main = print 4"
      ]
    ]
  ]
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File   fileName   _) = name == fileName

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, FSCrumb name ls rs : bs) =
  Just (Folder name (ls ++ [item] ++ rs), bs)
fsUp (_, []) = Nothing

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder folderName items, bs)
  | length ls == length items = Nothing
  | otherwise                 = Just (item, FSCrumb folderName ls rs : bs)
 where
  (ls, itemr) = break (nameIs name) items
  (item : rs) = itemr

fsRename :: Name -> FSZipper -> Maybe FSZipper
fsRename newName (Folder name items, bs) = Just (Folder newName items, bs)
fsRename newName (File   name dat  , bs) = Just (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile item (Folder folderName items, bs) =
  Just (Folder folderName (item : items), bs)

x -: f = f x

f :: Maybe a -> Either a ()
f (Just a) = Left a
f Nothing  = Right ()
