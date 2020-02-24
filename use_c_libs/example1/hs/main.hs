module Main(main) where

import Text.Printf
import Foo

main::IO()
main = do
    -- when return type is () the no call to printf
    -- let _ = f_info
    f_info
    _ <- f_info
    print $ f_foo 1 2
    print $ f_bar 3 1
    d0 <- f_nextData
    d1 <- f_nextData
    d2 <- f_nextData
    d3 <- f_nextData
    d4 <- f_nextData
    d5 <- f_nextData
    d6 <- f_nextData
    d7 <- f_nextData
    d8 <- f_nextData
    d9 <- f_nextData
    d10 <- f_nextData
    d11 <- f_nextData
    printf "%i, %i, %i, %i, %i, %i, %i, %i, %i, %i, %i, %i\n" d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11

