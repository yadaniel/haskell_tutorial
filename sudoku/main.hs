module Main(main) where

import System.IO
import Data.Char
import Data.Maybe
import Text.Printf

-- create :: Int -> [[Maybe Int]]
-- create n = [[(Nothing::Maybe Int) | _ <- [1..n]] | _ <- [1..n]]
--
-- pos :: (Int,Int) -> [[Maybe Int]] -> Maybe Int
-- pos (x,y) m = (m !! x) !! y
--
-- m = create 9
--
-- main = do
--     print m
--     print (pos (0,0) m)
--     print $ pos (0,0) m


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
    convert' _ = error "not full"

pos :: (Int,Int) -> [[Int]] -> Int
pos (x,y) m = (m !! x) !! y

set :: (Int,Int) -> [[Int]] -> [Int] 
set (r,c) g = let xs = map (\(r,c)-> g !! r !! c) $ [ (r,c') | c' <- [0..8] ] ++ [ (r',c) | r' <- [0..8] ] in
              let ys = filter (\x -> x /= 0) xs in
              uniq ys

uniq :: [Int] -> [Int]
uniq [] = []
uniq xs = uniq' [] xs where
    uniq' :: [Int] -> [Int] -> [Int]
    uniq' acc [] = acc
    uniq' acc (x:[]) = acc ++ [x]
    uniq' acc (x:xs) = uniq' (acc ++ [x]) (filter (\v -> v/=x) xs)
 
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
    





























