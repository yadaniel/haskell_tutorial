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
setRowCol :: (Int,Int) -> [[Int]] -> [Int] 
setRowCol (r,c) g = let xs = map (\(r,c)-> g !! r !! c) $ [ (r,c') | c' <- [0..8] ] ++ [ (r',c) | r' <- [0..8] ] in
                    let ys = filter (\x -> x /= 0) xs in
                    uniq ys

-- numbers taken from square
setSquare :: (Int,Int) -> [[Int]] -> [Int]
setSquare (r,c) g = let xs = map (\(r',c')-> g !! r' !! c') $ inSquare (r,c) in
                    uniq xs

-- indices from square
inSquare :: (Int,Int) -> [(Int,Int)]
inSquare (r,c) | r<0 || c<0 = error "outside range" 
               | r<3 && c<3 = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
               | r<3 && c<6 = [(0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)]
               | r<3 && c<9 = [(0,6),(0,7),(0,8),(1,6),(1,7),(1,8),(2,6),(2,7),(2,8)]
               | r<6 && c<3 = [(3,0),(3,1),(3,2),(4,0),(4,1),(4,2),(5,0),(5,1),(5,2)]
               | r<6 && c<6 = [(3,3),(3,4),(3,5),(4,3),(4,4),(4,5),(5,3),(5,4),(5,5)]
               | r<6 && c<9 = [(3,6),(3,7),(3,8),(4,6),(4,7),(4,8),(5,6),(5,7),(5,8)]
               | r<9 && c<3 = [(6,0),(6,1),(6,2),(7,0),(7,1),(7,2),(8,0),(8,1),(8,2)]
               | r<9 && c<6 = [(6,3),(6,4),(6,5),(7,3),(7,4),(7,5),(8,3),(8,4),(8,5)]
               | r<9 && c<9 = [(6,6),(6,7),(6,8),(7,6),(7,7),(7,8),(8,6),(8,7),(8,8)]
               | True = error "outside range"

-- numbers taken from row, column and square
setAll :: (Int,Int) -> [[Int]] -> [Int]
setAll (r,c) g = let xs = setRowCol (r,c) g in
                 let ys = setSquare (r,c) g in
                 let zs = xs ++ ys in
                 let vs = filter (\x -> x /= 0) zs in
                 uniq vs

setChoice :: (Int,Int) -> [[Int]] -> [Int]
setChoice (r,c) g = let all = setAll (r,c) g in
                    [ i | i <- [1..9], not $ elem i all ]

uniq :: [Int] -> [Int]
uniq [] = []
uniq xs = uniq' [] xs where
    uniq' :: [Int] -> [Int] -> [Int]
    uniq' acc [] = acc
    uniq' acc (x:[]) = acc ++ [x]
    uniq' acc (x:xs) = uniq' (acc ++ [x]) (filter (\v -> v/=x) xs)

solve :: [[Int]] -> [[[Int]]]
solve g = let idx = [(r,c) | c <- [0..9], r <- [0..9]] in
          -- next_soluton :: [[[Int]]] -> [(Int,Int)] -> [[[Int]]]
          next_solution [] idx where
          next_solution acc [] = acc
          next_solution acc ((r,c):idxs) = next_solution (acc ++ try_solution (setChoice (r,c) g)) idxs where
            try_solution = undefined
 
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
    let s1 = setRowCol (0,0) g1
    print s1
    -- print $ uniq [1,2,3,4,5,6,1,2,3,4,5,6,7]
    -- print $ inSquare (3,0)
    -- print $ inSquare (3,1)
    -- print $ inSquare (3,2)
    print $ setAll (0,0) g1
    print $ setChoice (0,0) g1
    return ()


