{-# LANGUAGE ExplicitForAll #-}

module Learnyoneda where

imager :: forall r. ((Bool -> r) -> [r])
imager iffie = fmap iffie [True, False, True, True]

data Color = Red | Green | Blue deriving (Show)

colorMap :: Bool -> Color
colorMap x = if x then Blue else Red

f :: Color -> String
f = show

main = do
  print $ imager (f . colorMap)
  print $ map f (imager colorMap)
