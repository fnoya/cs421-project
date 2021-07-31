{-# LANGUAGE DatatypeContexts #-}
module IS where

data Ord a => IS a = a :? IS a | E deriving (Eq, Show)

(<?) :: Ord a => a -> IS a -> IS a
(<?) = (:?)

(.<) :: Ord a => a -> IS a -> Bool
n .< E = False
n .< (x :? xs) = (n < x) || (n .< xs)

(.==) :: Ord a => a -> a -> Bool
x .== y = (x < y) && (y < x)


length :: [a] -> Int -> IS Int
length [] n = n :? E
length (x:xs) n = n :? IS.length xs (n+1)

minimum :: Ord a => [IS a] -> IS a
minimum = foldl1 minB

minimumD :: Ord a => IS a -> [IS a -> IS a] -> IS a
dfbb :: Ord a => IS a -> (IS a -> IS a) -> IS a
minimumD = foldl dfbb
dfbb u f = finalize (minB u (f u))

minB :: Ord a => IS a -> IS a -> IS a
minB E ys = E
minB xs E = E
minB xxs@(x :? xs) yys@(y :? ys)
    | x == y = x <? minB xs ys
    | x < y = x <? minB xs yys
    | otherwise  = y <? minB xxs ys

approx :: Ord a => IS a -> a
finalize :: Ord a => IS a -> IS a
approx (x :? xs) = x
finalize (x :? E) = x :? E
finalize (x :? xs) = finalize xs