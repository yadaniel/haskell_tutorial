{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE GADTs #-}

module Main(main) where

import qualified Data.Array as DA
import qualified System.Random as SR
import Text.Printf(printf)
import System.Win32 as SW32
import Data.Word as DW

-- set the default types
default (Int, Double)

-- data with types
data Q a b = Q0 | Q1 a | Q2 b | Q3 a b

-- data with selector
-- note: selectors have the type Q' -> a resp. Q' -> b
-- an exception is thrown when Q' cannot be unwrapped
-- r Q0'
-- no match in record selector r
data Q' a b = Q0' | Q1' {r::a} | Q2' {s::b} | Q3' {t1::a, t2::b}
exception_when_run _ = r Q0'

-- selectors are matching and can match different type constructors
data Q'' a b = Q0'' | Q1'' {p::a} | Q2'' {q::b} | Q3'' {p::a, q::b}
v1 = p (Q1'' {p=1})
v2 = p (Q3'' {p=1, q=2})
v3 = v1 == v2

l1 = [1|_ <- [1..10]]
l2 = take 10 $ repeat 1
-- all combinations, nested loop
lx1 = [(x,y)| x<-[1..10],even x, y<-[1..10],odd y]
-- like zip, process loop in parallel
lx2 = [(x,y)| x<-[1..10],even x | y<-[1..10],odd y]
--
lx3 = zip [x|x<-[1..10],even x] [y|y<-[1..10],odd y]

lst = [(i,i^2)| i<-[1..10], then reverse]

-- tm :: Int -> IO(WORD, WORD, WORD, WORD)
-- tm _ = SW32.getLocalTime >>= \x -> return (wHour x, wMinute x, wSecond x, wMilliseconds x)

-- no need to have dummy parameter (compare to f#)
tm :: IO(WORD, WORD, WORD, WORD)
tm = SW32.getLocalTime >>= \x -> return (wHour x, wMinute x, wSecond x, wMilliseconds x)

fillRandom :: Int -> IO [Int]
fillRandom n = do 
    tm <- SW32.getLocalTime
    let seed = (fromIntegral (SW32.wMilliseconds tm)) :: Int
    let gen = SR.mkStdGen seed
    return (rand n gen []) where
        rand 0 gen xs = xs
        rand n gen xs = let (x,gen') = (SR.random gen) :: (Int,SR.StdGen) in (rand (n-1) gen' (x:xs))

fillRandomRange :: Int -> Int -> Int -> IO [Int]
fillRandomRange n a b = do
    xs <- fillRandom n 
    let ys = [(mod x (b-a)) + a | x <- xs]
    return ys

-- fillRandomRange :: Int -> Int -> Int -> IO [DW.Word8]
-- fillRandomRange n a b = do
--     xs <- fillRandom n 
--     let ys = [(fromIntegral (mod x (b-a))) :: DW.Word8 | x <- xs]
--     return ys

main :: IO()
main = do
        print "main"
        -- exception_when_run ""
        print v3
        printf "%s\n" (show lx1)
        printf "%s\n" (show lx2)
        printf "%s\n" (show lx3)
        printf "%s\n" (show lst)
        --
        SW32.sleep 1000
        -- tm_ <- tm 1
        tm_ <- tm
        printf "%s\n" (show tm_)
        --
        SW32.sleep 1000
        -- tm_ <- tm 1
        tm_ <- tm
        printf "%s\n" (show tm_)
        --
        xs <- (fillRandom 10)
        printf "%s\n" (show xs)
        --
        ys <- (fillRandomRange 100 50 80)
        printf "%s\n" (show ys)
        --
        return ()


