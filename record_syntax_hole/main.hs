module Main(main) where

import Text.Printf(printf)
import System.Environment as SEnv
import System.Exit

-- ghci
-- :cd "C:\\Users\\user\\Desktop\\gitRepos\\00_githut\\haskell_tutorial\\record_syntax"
-- :! cd
-- :load main.hs

-- cat C:\\Users\\user\\AppData\\Roaming\\ghc\\ghci.conf
-- :set -XGADTs
-- :set -XParallelListComp
-- :set -iA:B:C

-- ghc
-- ghc -fobject-code main.hs

data X a b = X' {x1::a, x2::b} deriving (Eq, Show)
-- X' :: a -> b -> X a b
-- x1 :: X a b -> a
-- x2 :: X a b -> b

-- usage
t1 = X' {x1 = 1, x2 = 1.0}
t2 = X' {x1 = 1, x2 = 2.0}
t3 = t1 {x2 = 2.0}

-- hole driven development
splitPath :: String -> [String]
splitPath xs = splitPath' xs "" [] where
    splitPath' [] path paths = paths ++ [path]
    splitPath' (x:xs) path paths =  case x of
        ';' -> splitPath' xs "" (paths ++ [path])
        _ -> splitPath' xs (path ++ [x]) paths

-- splitPath :: String -> [String]
-- splitPath xs = splitPath' xs "" [] where
--     splitPath' [] path paths = paths ++ [reverse path]
--     splitPath' (x:xs) path paths =  case x of
--         ';' -> splitPath' xs "" (paths ++ [reverse path])
--         _ -> splitPath' xs (x:path) paths

foo = 1.0
bar = True

foobar :: (Int, Double) -> Bool -> Int
foobar (_,_) _ = 1

main = do
    print "in main"
    printf "%s\n" $ show t1
    printf "%s\n" $ show t2
    printf "%s\n" $ show t3
    printf "%s\n" $ show (t2 == t3)
    --
    path <- getEnv "PATH"
    printf "%s\n" path
    printf "%s\n" $ map (\x -> case x of ';' -> '\n'; _ -> x) path
    printf "%s\n" $ map (\x -> if x == ';' then '\n' else x) path
    printf "%s\n" $ show $ splitPath path
    printf "%s\n" $ show ["foo","bar"]
    --
    -- two holes
    -- suggests foo for _a 
    -- suffests bar for _b
    -- uncomment following line to test
    -- return $ foobar (1,_a) _b
    --
    return 1
    pure 2
    let x = 1 in
        let y = 2 in
        exitWith (ExitFailure (x+y))
    -- pure ()
    -- not needed, bevause exitWith -> IO a

