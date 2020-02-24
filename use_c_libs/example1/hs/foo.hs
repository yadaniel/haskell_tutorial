module Foo(f_foo, f_bar, f_info, f_nextData) where

import Foreign
import Foreign.C.Types
import Data.Coerce (coerce)

-- ccall = c default
-- stdcall = winapi

-- foreign import ccall unsafe "info" c_info :: IO ()
-- foreign import ccall safe "info" c_info :: IO ()
foreign import ccall "info" c_info :: IO ()
-- foreign import ccall "info" c_info :: ()
f_info = c_info

-- foreign import ccall "nextData" c_nextData :: CInt
foreign import ccall "nextData" c_nextData :: IO CInt
f_nextData :: IO Int
f_nextData = fmap fromIntegral c_nextData

foreign import ccall "foo" c_foo :: CInt -> CInt -> CInt
f_foo :: Int -> Int -> Int
f_foo x1 x2 = fromIntegral $ c_foo (fromIntegral x1) (fromIntegral x2)

foreign import ccall "bar" c_bar :: CDouble -> CDouble -> CDouble
f_bar :: Double -> Double -> Double
f_bar x1 x2 = coerce $ c_bar (coerce x1) (coerce x2)
-- f_bar x1 x2 = realToFrac $ c_bar (realToFrac x1) (realToFrac x2)

