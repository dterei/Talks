module Hs2Core where

import Prelude hiding (id, map, foldr, Maybe(..))

f g = let x = 2 + 2
      in (g x, x)

idChar :: Char -> Char
idChar c = c

id :: a -> a
id x = x

idChar' :: Char -> Char
idChar' = id

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

data Maybe a = Nothing | Just a

add :: Int -> Int -> Int
add x y = x + y

add2 :: Int -> Int
add2 = add 2

add4 :: Int -> Int
add4 z = (add2 2) + z

class MyEnum a where
	toId  :: a -> Int
	fromId :: Int -> a

instance MyEnum Int where
	toId = id
	fromId = id

instance (MyEnum a) => MyEnum (Maybe a) where
	toId (Nothing) = 0
	toId (Just n)  = 1 + toId n
	fromId 0       = Nothing
	fromId n       = Just (fromId $ n - 1)

showId :: MyEnum a => a -> String
showId n = "Enum value: " ++ (show $ toId n)

g :: IO ()
g = do
	putStrLn "Hello World"
	putStrLn "What's up today?"

h :: Int -> IO ()
h 0 = putStrLn "No go!"
h n = do
	putStrLn $ "I'm only going to ask you " ++ show n ++ " more times!"
	putStrLn "What is the magic number?"
	m <- getChar
	case m of
		'7' -> putStrLn "ftw!"
		_   -> h (n - 1)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ y []     = y
foldr f y (x:xs) = f x (foldr f y xs)

add1 :: [Int] -> [Int]
add1 []     = []
add1 (x:xs) = x + 1 : add1 xs

