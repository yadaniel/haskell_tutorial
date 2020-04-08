{-# LANGUAGE BangPatterns #-}

module Main where

-- because of the lazy evaluation
-- haskell will build up chain of unevaluated expressions (thunks)
-- this may avoid unnecessary calculations and improve overall runtime 
-- this may eat up the memory and impede overall runtime
-- in such case strict evaluation may be applied explicitly ... seq, $!

default (Int, Float)

-- seq :: a -> b -> b
-- takes a and b, evaluates a then returns b
-- try to evaluate strict, however there are evaluation barrier (data constructor, lambda)
--
-- x1 = seq undefined 1
x2 = seq (\x -> undefined) 1

-- f $! x = x `seq` f x
-- ($!) :: (a->b) -> a -> b
x3 = ($!) (+) 1 1

add :: (Num a) => a -> a -> a
add x1 x2 = x1 + x2

mult :: (Num a) => a -> a -> a
mult x1 x2 = x1 * x2

-- ($)  :: (a->b)->a->b
-- ($!) :: (a->b)->a->b
x4 = add 1 $ mult 2 3
x5 = add 1 $! mult 2 3
x4' = ($) (add 1) (mult 2 3)
-- calculate (mult 2 3) strict
x5' = ($!) (add 1) (mult 2 3)

x6 = mult 2 $ add 1 3
x7 = mult 2 $! add 1 3
x6' = ($) mult 2 (add 1 3)
-- calculate (add 1 3) strict
x7' = ($!) mult 2  (add 1 3)

-- add 1 :: (Num a) => a -> a
-- mult 2 :: (Num a) => a -> a

x8= add 1 $ mult 2 3

data E = E0 | E1 Int
instance Show E where
    show E0 = "[E0]"
    show (E1 x) = "[E1]" ++ (show x)

-- :k Int = *
-- :k A = * -> *
-- :k A Int = *
data A a = A0 | A1 a
instance (Show a) => Show (A a) where
    show A0 = "[A0]"
    show (A1 x) = "[A1]" ++ (show x)

-- BangPatterns allow strict evaluation of parameters given to the match
f1 x y = (x,y) 
f2 x !y = (x,y)
f3 !x y = (x,y)
f4 !x !y = (x,y)

-- without BangPatterns
p1 x y = (x,y)
p2 x y = (x, seq y y)
p3 x y = (seq x x,y)
p4 x y = (seq x x, seq y y)


-- note: haskell is super lazy
-- f1 !x !y = (x,y)
-- f2 x y = (seq x x, seq y y)
-- f1 1 undefined
-- f2 1 undefined
-- both show undefined error message, however
-- q = f1 1 undefined
-- q = f2 1 undefined
-- show no undefined error message, because q builds a trunk which is not evaluated yet
-- when forcing q to print out with
-- q
-- will show undefined error message

main = do
    -- print x1
    print x2
    print x3
    print x4
    print x5
    print x6 
    print x7
    print E0
    print (E1 1)
    -- ambigious type variable
    -- print A0
    print (A0 :: (A Int))
    print (A1 1)
    return ()

