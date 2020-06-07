{-# LANGUAGE GADTs, CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
{-# OPTIONS_GHC -Wtyped-holes #-}
{-# OPTIONS_GHC -Wname-shadowing #-}
{-# OPTIONS_GHC -Winaccessible-code #-}
{-# OPTIONS_GHC -Woverflowed-literals #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Woverlapping-patterns #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}

module Main(main) where

import qualified Text.ParserCombinators.Parsec as PC
import qualified Text.Parsec as P
import Control.Applicative
import Control.Monad
import Text.Printf(printf)
import Data.Word(Word8, Word16)

{-# INLINE foo #-}
foo = \x -> \y -> \z -> x+y*z

_ = 1::Word8
-- warning
-- _ = 256::Word8
-- no warning, wrapped around 0
_ = (2^8)::Word8

val :: String -> Int
val s = case (P.parse (P.many P.digit) "err" s) of
            (Left _) -> 0
            (Right v) -> (read v)

val' :: String -> Int
val' s = case (PC.parse (PC.many PC.digit) "err" s) of
            (Left _) -> 0
            (Right v) -> (read v)

var :: String -> String
var s = case (P.parse (P.many P.letter) "err" s) of
            (Left _) -> ""
            (Right v) -> v

var' :: String -> String
var' s = case (PC.parse (PC.many PC.letter) "err" s) of
            (Left _) -> ""
            (Right v) -> v

-- arrs = do
--     _ <- P.char '['
--     _ <- P.many1 P.spaces
--     _ <- P.char ']'
--     return ()

-- match string "true"
matchTrue :: PC.Parser String
matchTrue = PC.string "true"

-- return wrapped True
alwaysTrue :: PC.Parser Bool
alwaysTrue = pure True

-- match string "true" and when matched return wrapped True
boolTrue :: PC.Parser Bool
boolTrue = matchTrue *> alwaysTrue

-- short version for false
boolFalse :: PC.Parser Bool
boolFalse = PC.string "false" *> pure False

-- combine the parsers
bool :: PC.Parser Bool
bool = boolTrue PC.<|> boolFalse

-- combined data structure
data Vals = B Bool | S String | A [Vals] deriving (Show)

array :: PC.Parser Int
array = do
    _ <- PC.char '['
    _ <- PC.many PC.space
    v <- PC.many PC.digit
    _ <- PC.many PC.space
    _ <- PC.char ']'
    return (case v of {"" -> 0; v' -> (read v')})

array' :: PC.Parser (Maybe Int)
array' = do
    _ <- PC.char '['
    _ <- PC.many PC.space
    v <- PC.many PC.digit
    _ <- PC.many PC.space
    _ <- PC.char ']'
    return (case v of {"" -> Nothing; v' -> Just (read v')})
    -- Left (Maybe Int)
    -- Right (Maybe Int)

test :: IO()
test = do
    printf "in test\n"
    let _ = 1
    return ()

test1 :: String -> IO Int
test1 s = do
    let v = PC.parse array "err" s
    let i = case v of { Left _ -> 0; Right v' -> v' }
    return i

test1' :: String -> IO (Maybe Int)
test1' s = do
    let v = PC.parse array' "err" s
    let i = case v of { Left _ -> Nothing; Right v' -> case v' of {Nothing -> Nothing; Just v'' -> Just v''} }
    return i


main :: IO()
main = do
    print $ val "1234"
    print $ val' "1234"
    print $ var "foo"
    print $ var' "foo"
    let v = PC.parse boolTrue "err" "true"
    let w = PC.parse boolFalse "err" "false"
    printf "%s,%s\n" (show v) (show w)
    let vw = PC.parse bool "err" "true"
    let vw' = PC.parse bool "err" "false"
    printf "%s,%s\n" (show vw) (show vw')
    test
    x <- test1 "[]"
    -- x <- test1 "[ 1 ]"
    -- x <- test1 "[1]"
    printf "parsed %i\n" $ x
    --
    x' <- test1' "[]"
    -- x' <- test1' "[1]"
    printf "parsed %s\n" $ (case x' of {Nothing -> "..."; Just x'' -> (show x'')})

    return ()

