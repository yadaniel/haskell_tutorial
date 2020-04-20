module Main(main) where

newtype X = X {x::Int}

-- all option OK
instance Show X where
    show (X x') = show x'
    -- show s = show . x $ s
    -- show = show . x

newtype Y a = Y {y::a}

instance Show a => Show (Y a) where
    show = show . y 

main = do
    print "in main"
    print $ X {x=1}
    print $ Y {y=1}

