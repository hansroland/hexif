-- -----------------------------------------------------------------------------
-- Utils.hs  Little support functions for the hexif project
-- -----------------------------------------------------------------------------

module Utils where

import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)

-- little support functions: normal pack/unpack are refused by GHC
-- Hoogle says: unpack :: BL.ByteString -> String
-- GHCi says:   unpack ::  BL.ByteString -> [Word8]
-- In reality we converts 8-bit bytes to identical 8-bit bytes!
-- Why is Haskell string conversion so difficult?
unpackLazyBS :: BL.ByteString -> String
unpackLazyBS = map (chr . fromIntegral)  . BL.unpack
