module Main where

import System.Hardware.Serialport as SP
import Text.Printf

-- ghci seems to work and read 5 chars
-- ghc version returns only 1 char
--
-- Prelude SP TP> s <- openSerial "COM42" defaultSerialSettings {commSpeed = CS9600}
-- Prelude SP TP> q <- recv s 5 >>= print
-- "\170\192<\NUL\181"
-- Prelude SP TP> closeSerial s

main = do
    printf "trying to open serial port ..."
    s <- openSerial "COM42" defaultSerialSettings { commSpeed = CS9600 }
    -- s <- openSerial "COM42" defaultSerialSettings { commSpeed = CS9600, timeout = 1 }
    -- s <- openSerial "COM42" defaultSerialSettings { commSpeed = CS9600, timeout = 100 } -- 10s
    xs <- (recv s 5) >>= print
    -- print xs
    -- printf "%s" xs
    closeSerial s
    printf "... exiting"


-- This module provides the serial port interface.
--
--  import qualified Data.ByteString.Char8 as B
--  import System.Hardware.Serialport
--  let port = "COM3"          -- Windows
--  let port = "/dev/ttyUSB0"  -- Linux
--  s <- openSerial port defaultSerialSettings { commSpeed = CS2400 }
--  send s $ B.pack "AT\r"
--  recv s 10 >>= print
--  closeSerial s

-- -- Or use the experimental interface with standard handles:
-- 
--  import System.IO
--  import System.Hardware.Serialport
--  let port = "COM3"           -- Windows
--  let port = "/dev/ttyUSB0"   -- Linux
--  h <- hOpenSerial port defaultSerialSettings
--  hPutStr h "AT\r"
--  hGetLine h >>= print
--  hClose h

