module Main(main) where

import Data.Bits as DBits
import Text.Printf(printf)
-- import Types
-- import Types(Q(..))
-- import Types(Q(..), V(..), W(..))
-- import Types(Q(..), V(V0), W(W0))
import Types(Q(..), V(V0), W(..))

-- :k Q 
-- *->*->*
foo :: Q a b -> Int
foo Q0 = 0
foo (Q1 _) = 1
foo (Q2 _) = 2
foo (Q3 _ _) = 3

bar :: W a b -> Int
bar W0 = 0
-- bar (W1 _) = 1
-- W1 not exported, so even W(..) will not import it

clearBits :: Int -> [Int] -> Int
clearBits val [] = val
clearBits val (b:bs) = clearBits (clearBit val b) bs

main :: IO()
main = do
    printf "in main\n"
    printf "%i\n" $ DBits.shift (1::Int) 10
    printf "%s\n" $ show $ DBits.shift (1::Int) 10
    printf "%i\n" $! DBits.shift (1::Int) 10
    printf "%s\n" $! show $! DBits.shift (1::Int) 10
    --
    printf "%i\n" ((DBits.shift 1 10)::Int)
    printf "%i\n" ((DBits.shiftL 1 10)::Int)
    printf "%i\n" ((DBits.shiftR 1024 10)::Int)
    printf "%i\n" ((DBits.clearBit 0xFF 7)::Int)
    printf "%i\n" (clearBits 0xFF [7,1,0])
    printf "%i\n" (((.&.) 0xFF 0xFF)::Int)
    printf "%i\n" (((DBits..&.) 0xFF 0xFF)::Int)
    printf "%i\n" ((DBits.bit 100)::Int)
    printf "%i\n" ((DBits.bit 100)::Integer)
    printf "%i\n" (DBits.popCount (0x1FF::Int))
    printf "%i\n" ((DBits.popCount . DBits.complement) (0x1FF::Int))
    printf "%i\n" (DBits.popCount (0x1F0::Int))
    printf "%i\n" ((DBits.popCount . DBits.complement) (0x1F0::Int))
    printf "%i\n" ((DBits.complementBit 15 0)::Int)
    --
    -- print $ DBits.shift (1::Int) 10
    return ()

