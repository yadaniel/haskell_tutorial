{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import qualified Language.C.Inline as C

C.include "<stdio.h>"
C.include "<math.h>"

mycos :: CDouble -> IO CDouble
mycos x = [C.exp| double{ cos($(double x)) } |]

main :: IO ()
main = do
   x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
   putStrLn $ show x ++ " characters printed."


