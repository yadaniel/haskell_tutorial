module Main(main) where

import System.IO
import Data.Char
import Data.Maybe
import Text.Printf

-- Grid 01
-- 003020600
-- 900305001
-- 001806400
-- 008102900
-- 700000008
-- 006708200
-- 002609500
-- 800203009
-- 005010300
--
-- Grid 01
-- 003|020|600
-- 900|305|001
-- 001|806|400
--
-- 008|102|900
-- 700|000|008
-- 006|708|200
--
-- 002|609|500
-- 800|203|009
-- 005|010|300

type GameIndex = Int

readGame :: FilePath -> GameIndex -> IO [String]
readGame fname idx = do 
    txt <- readFile fname
    let games = lines txt 
    let game = dropWhile (\line -> line /= "Grid " ++ (printf "%02i" idx)) games
    let ln = take 10 game
    return ln

convert :: [String] -> [[Int]]
convert [] = []
convert (x:xs) = convert' x ++ (convert xs) where
    convert' (x1:x2:x3:x4:x5:x6:x7:x8:x9:[]) = [[d x1, d x2,d x3,d x4,d x5,d x6,d x7,d x8,d x9]] where d = digitToInt
    convert' _ = error "format"

pos :: (Int,Int) -> [[Int]] -> Int
pos (x,y) m = (m !! x) !! y

-- numbers taken from row and column
set :: (Int,Int) -> [[Int]] -> [Int] 
set (r,c) g = let xs = map (\(r,c)-> g !! r !! c) $ [ (r,c') | c' <- [0..8] ] ++ [ (r',c) | r' <- [0..8] ] in
              let ys = filter (\x -> x /= 0) xs in
              uniq ys

-- numbers taken from square
set1 :: (Int,Int) -> [[Int]] -> [Int]
set1 (r,c) _ = let r' = r//3 in
             let c' = c//3 in
             []

uniq :: [Int] -> [Int]
uniq [] = []
uniq xs = uniq' [] xs where
    uniq' :: [Int] -> [Int] -> [Int]
    uniq' acc [] = acc
    uniq' acc (x:[]) = acc ++ [x]
    uniq' acc (x:xs) = uniq' (acc ++ [x]) (filter (\v -> v/=x) xs)
 
main :: IO()
main = do
    game1 <- readGame "data" 1
    game2 <- readGame "data" 2
    print game1
    print game2
    let g1 = convert $ drop 1 game1
    let g2 = convert $ drop 1 game1
    print g1
    print g2
    let v1 = pos (0,0) g1
    let v2 = pos (1,0) g1
    print v1
    print v2
    let s1 = set (0,0) g1
    print s1
    -- print $ uniq [1,2,3,4,5,6,1,2,3,4,5,6,7]
    return ()
    





























