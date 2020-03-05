module A where

-- import GHC.Base (plusInt)

-- | Factorial with accumulator
afac :: Int -> Int -> Int
afac a 0 = a
afac a n = afac (n*a) (n-1)

g :: Num a => a -> a
g x = x + x * 4

h :: Int
h = g 5

k :: Int -> Int
k = g

l :: Int -> Int
l n = g n * 2

add :: Int -> Int -> Int
add x y = x + y + 2

-- x :: Int -> Int
-- x z = plusInt 2 (id z)

id' :: a -> a
id' x = x

build_data :: Int -> Maybe Int
build_data x = Just (x + 1)

build_terei :: a -> Maybe a
build_terei x = Just x

case_terei :: Maybe Int -> Int
case_terei x = case x of Just x -> x; Nothing -> 10

