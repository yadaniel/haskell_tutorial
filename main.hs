module Main (combine, main) where

-- stub
combine :: int -> int -> (int,int)
combine x y = error "not implemented"

-- declaration style
mymap1 :: (a->b) -> [a] -> [b]
mymap1 f [] = [] 
mymap1 f (x:xs) = (f x) : (mymap1 f xs)

-- using case of 
mymap2 f xs =
    case xs of
        [] -> []
        (x:xs) -> f x : mymap2 f xs

-- using conditional guards
mymap3 f xs
    | (xs == []) = []
    | True = f (head xs) : mymap3 f (tail xs)

-- using if
mymap4 f xs =
    if xs == [] then []
    else f (head xs) : mymap4 f (tail xs)

x1 = mymap1 (\x -> 2*x) [1..5]
x2 = mymap2 (\x -> 2*x) [1..5]
x3 = mymap3 (\x -> 2*x) [1..5]
x4 = mymap4 (\x -> 2*x) [1..5]


-- using nested if/else
foo :: (Ord int, Num int) => int -> int -> int -> int
foo x y z = if x > 100 then y else if x > 10 then z else 0 

-- notes on IO functions
-- putStr / putStrLn functions output string to console
-- print function output any value to console

data XYZ = X | Y | Z deriving (Eq, Ord, Show)


main :: IO()
main = do
    print X
    print (X<Z)
    print (X /= X)
    print x1
    print x2
    print x3
    print x4
    print [1,2,3]
    putStr "test\n"
    putStrLn "test"
