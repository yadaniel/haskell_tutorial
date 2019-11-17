module Main where

--
-- data = algebraic data types
--

-- enumeration without data
data S = S1 | S2 | S3 | S4

-- enumeration with and without data (discriminated union)
data S_ = S1_ Int | S2_ Double | S3_ () | S4_

-- enumeration with type parameters
data S__ a b = S1__ a | S2__ b

-- product type

-- record syntax
data P = P1 { p1::Int, p2::Double }

-- non-record syntax
data P_ = P1_ Int Double

-- sum and product types
data SP a b c = SP1 | SP2 {x1::a, x2::b, x3::c} | SP3 Int | SP4 Double | SP5 () | SP6 a | SP7 b | SP8 c

--
-- type = alias
--

type QInt = Int
type QBool = Bool
type QDouble = Double

t1 = 1::QInt
t2 = True::QBool
t3 = 1.0::QDouble

--
-- newtype = exactly one data constructor with exactly one field
--

-- newtype F = F1 Int Int
newtype F = F1 Int
-- newtype F = F1
newtype R = R1 ()

main::IO()
main = do putStrLn "main"

