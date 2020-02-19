module Main(main) where

import Data.Char
import Data.List
import Text.Printf
import System.Environment

-- print :: (Show a) => a -> IO()

main :: IO()
-- main = error ""
-- main = undefined
-- main = return ()
-- main = do print ""
-- main = print "test"
-- main = getLine >>= \x -> print x
-- main = getLine >>= \x -> print x >>= \y -> return ()
-- main = getLine >>= \x -> putStrLn x >>= \_ -> getLine >>= \y -> putStrLn y
-- main = getLine >>= (\x -> putStrLn x >>= (\_ -> getLine >>= (\y -> putStrLn y)))
-- main = getLine >>= (\x -> putStrLn x >>= (\_ -> getLine >>= (\y -> putStrLn y)))
main = do { x <- getLine; putStrLn x; y <- getLine; putStrLn y }


