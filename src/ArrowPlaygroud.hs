{-# LANGUAGE GADTs #-}

module ArrowPlaygroud where
   
import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.IO

data LI_Gadt1 where
  {MkShow1 :: a -> (a -> String) -> LI_Gadt1}

data LI_Gadt2 where
  {MkShow2 :: Show a => a -> LI_Gadt2}


showGadt2 (MkShow2 v) = show v
main = do
  let f = Kleisli print
          . arr length
          . arr words
          . Kleisli readFile
  runKleisli f "jabberwocky.txt"
