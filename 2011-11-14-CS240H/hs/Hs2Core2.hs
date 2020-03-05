module Hs2Core2 where

import Prelude
import Data.List

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x
                    in z' `seq` foldl' f z' xs
-}

forccee = seq

dox :: Int -> Int
dox n = x * x
    where x = (n + 2) * 4

iff :: Bool -> a -> a -> a
iff True  x _ = x
iff False _ y = y

ifn :: Int -> Int -> Int
ifn 0 x = x
ifn 1 x = x * 1
ifn 2 x = x * 2
ifn _ x = x * 4

sum100n :: Int -> Int
sum100n n = n * foldr (+) 0 [1..100]

sum100l :: Int
sum100l = foldl (+) 0 [1..100]

sum100l' :: Int
sum100l' = foldl' (+) 0 [1..100]

g :: (Double -> Double -> Double) -> Double -> Double -> Double
g f x y = f (sin x) y


