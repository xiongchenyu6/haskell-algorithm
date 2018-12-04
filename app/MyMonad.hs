module MyMonad where

import Control.Monad

newtype Writer w a = Writer {runWriter:: (a, w)} deriving (Show)

instance (Monoid w) => Functor (Writer w) where
    fmap f writer = let (a, w) = runWriter writer in
                    Writer $ (f a,w)

instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    writerF <*> writerV = let (f ,w ) = runWriter writerF
                              (v ,w') = runWriter writerV
                              in Writer (f v, mappend w' w)

instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    writer >>= f = let (a, w) = runWriter writer
                       (a',w') = runWriter $ f a in
                       Writer (a', mappend w' w)

main :: IO()
main = do
    putStrLn "test"
    print $ fmap f z
    where
        z :: Writer String String
        z = Writer ("aa", "bb")
        f = length