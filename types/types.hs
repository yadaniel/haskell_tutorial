-- module Types where
-- module Types(Q(Q0,Q1,Q2,Q3)) where
-- module Types(Q(Q0,Q1,Q2)) where
-- module Types(Q(..),V(..),W(..)) where
module Types(Q(Q0,Q1,Q2,Q3), V(..), W(W0)) where

-- possible to export only subset of data constructors

data Q a b = Q0 | Q1 a | Q2 b | Q3 a b

data V a b = V0 | V1 {v1a::a} | V2 {v2b::b}  | V3 {v3a::a, v3b::b}

data W a b = W0 | W1 {r::a} | W2 {s::b}  | W3 {r::a, s::b}

data G = G0 | G1 | G2 | Q4

