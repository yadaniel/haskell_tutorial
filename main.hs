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
-- int as used below is type variable and not an actual type
-- thus adding of type classes (Ord and Num) were required
--foo :: (Ord int, Num int) => int -> int -> int -> int
foo :: Int -> Int -> Int -> Int
foo x y z = if x > 100 then y else if x > 10 then z else 0 

-- notes on IO functions
-- putStr / putStrLn functions output string to console
-- print function output any value to console

-- simple enumeration with equality, ordering and convertable to string
data XYZ = X | Y | Z deriving (Eq, Ord, Show)

-- cartesian and polar complex representations in record syntax 
data ComplexR = CR {x::Double, y::Double} | PR {r::Double, phi::Double}
    deriving (Eq, Show)

-- cartesian and polar complex representations in non-record syntax 
data Complex = C Double Double | P Double Double
    deriving (Eq, Show)

p2c :: Complex -> Complex
p2c (P r phi) = C (r*(cos phi)) (r*(sin phi))

c2p :: Complex -> Complex
c2p (C x y) = P (sqrt(x*x + y*y)) (atan2 y x)

-- complex addition based on data type with the non-record syntax
addC :: Complex -> Complex -> Complex
addC (C x1 y1) (C x2 y2) = C (x1+x2) (y1+y2)
addC p1 p2 = C (x1+x2) (y1+y2)
    where 
        C x1 y1 = p2c p1 
        C x2 y2 = p2c p2

-------------------------------------------
-- the name used in the record can be skipped when doing pattern matching 
--
p2cR :: ComplexR -> ComplexR
p2cR (PR r phi) = CR (r*(cos phi)) (r*(sin phi))

c2pR :: ComplexR -> ComplexR
c2pR (CR x y) = PR (sqrt(x*x + y*y)) (atan2 y x)

-- complex addition based on data type with the record syntax
addCR :: ComplexR -> ComplexR -> ComplexR
addCR (CR x1 y1) (CR x2 y2) = CR (x1+x2) (y1+y2)
addCR p1 p2 = CR (x1+x2) (y1+y2)
    where 
        CR x1 y1 = p2cR p1 
        CR x2 y2 = p2cR p2


-- simple wrapper for generic type a
data Val a = Val {val::a} deriving (Show)


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
