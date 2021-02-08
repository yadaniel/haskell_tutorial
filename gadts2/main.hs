{-# LANGUAGE GADTs #-}

module Main where

-- data type as enum
data AX = A0 | A1 | A2 | A3 deriving (Eq, Show)
a0 = A0::AX
a1 = A1::AX
a2 = A2::AX
a3 = A3::AX

-- data type as discriminated variant
data BX a b = B0 | B1 a | B2 b | B3 a b deriving Show
b0 = B0
b0' = B0 :: (BX a b)
b1 = B1 1
b1' = (B1 1) :: (BX Int b)
b2 = B2 1.0
b2' = (B2 1.0) :: (BX a Double)
b3 = B3 1 1.0
b3' = (B3 1 1.0) :: (BX Int Double)

-- newtype has 2 constraints
-- it must have one type argument, which is a
-- it must have one type constructor, which is V
newtype T1 a = V a
newtype T2 a = W (a,a)  -- tuple counts as one field
newtype T3 a = VW [a]

-- type makes alias
type Index = Int
type Point3D = (Double,Double,Double)

-- unnamed field record
data Addr = Addr String String

-- named field record
data Address = Address { fname::String, lname::String }
-- fname :: Address -> String
-- lname :: Address -> String

data Container a = C { name::String, value::[a] }

-- unnamed and named records combined
type Ref = Int
data Expr = Num Ref (Either Int Double) | Name { ref::Ref, n::String} | Maybe ()

-- gadt
data Maybe2 a where
    Just2 :: a -> Maybe2 a
    Nothing2 :: Maybe2 a

data Either2 a b where
    Left2 :: a -> Either2 a b
    Right :: b -> Either2 a b

main :: IO()
main = do
    print "main"
    return ()

