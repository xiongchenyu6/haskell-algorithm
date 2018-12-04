{-# LANGUAGE TemplateHaskell #-}

module ConstSplices where
import Language.Haskell.TH
import Mysplices
import Control.Monad (forM)
import Data.List
import Language.Haskell.TH.Quote


constN :: Int -> Q Dec
constN nth = do
    expr<- constExp nth
    let name = mkName $ "const" ++ show nth
    return $ FunD name [ Clause [myPat] (NormalB expr) [myDec] ]

constExp :: Int -> Q Exp
constExp nth = do
    a <- newName"a"
    return $ LamE (VarP a : replicate nth WildP) (VarE a)

deriveSetters :: Name -> Q [Dec]
deriveSetters nm = do
    TyConI tyCon<- reify nm
    case tyCon of
      DataD _ nm tyV ars cs  _ -> do
          let fieldsTypes = nub (concatMap recFields cs) -- (3)
          forM fieldsTypes $
              \(nm, ty) ->setterDec nm                   -- (5)
  where
    recFields (RecC _ xs) =                              -- (4)
        map (\(var,_,ty) -> (var, ty)) xs

setterDec :: Name -> Q Dec
setterDec nm = do
    let nmD = mkName $ nameBase nm ++ "'"-- (2)
    nmV<- newName"val"
    nmP<- newName"p"
    let pat  = [VarP nmV, VarP nmP]
        body = NormalB $ RecUpdE (VarE nmP) [ (nm, VarE nmV) ] -- (4)
    return $ FunD nmD [ Clause pat body [] ]               --(1)
