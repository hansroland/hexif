-- | Little support functions for the hexif project.
-- This module is an internal module of Graphics.Hexif and should only be used in the hexif project!


module Graphics.Hexif.Utils where

import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)
import Data.List (intersperse)

-- | Convert a lazy ByteString into a normal Haskell String
-- Normal pack/unpack are refused by GHC
-- Hoogle says: unpack :: BL.ByteString -> String
-- GHCi says:   unpack ::  BL.ByteString -> [Word8]
-- In reality we converts 8-bit bytes to identical 8-bit bytes!
-- Why is Haskell string conversion so difficult?
unpackLazyBS :: BL.ByteString -> String
unpackLazyBS = map (chr . fromIntegral)  . BL.unpack

