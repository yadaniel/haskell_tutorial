{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- deprecated
-- {-# LANGUAGE DatatypeContexts #-}
-- data (Show a) => D a = D0 | D1 a | D2 a a | D3 a a a deriving Show

module Main where

data Y a where
    YInt :: Int -> Y Int
    YFloat :: Float -> Y Float
    YDouble :: Double -> Y Double
    YT :: { arg1::Int, arg2::Int }  -> Y (Int,Int)

deriving instance Show (Y a)

main1 = do
    print "Y a"

--
--
--

data X a = X0 | X1 a | X2 a a | X3 a a a deriving (Eq, Ord, Show)

x0 = X0
x1 = X1 1
x2 = X2 1 10
x3 = X3 1 10 100

equals :: (Eq a) => X a -> X a -> Bool
equals v1 v2 = v1 == v2

bigger :: (Ord a) => X a -> X a -> Bool
bigger v1 v2 = v1 > v2

test v1 v2 = v1 == v2
-- test v1 v2 = v1 > v2

-- main :: IO()
-- main = error "main"
-- main = undefined
main = do
    -- print $ (X0) -- ambigious without type parameter
    print $ (X0::X Int) -- type parameter added
    print (X0::X Int)
    print $ X1 1
    print (X1 (1::Float))
    print ((X1 1)::X Float)
    -- print $ ((X1 1)::(X Float))
    print $ test x0 x1
    print $ test x0 (X0::X Int)
    print $ test x0 x2
    main1



















