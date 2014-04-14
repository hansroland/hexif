-- -----------------------------------------------------------------------------
-- Utils.hs  Little support functions for the hexif project
-- -----------------------------------------------------------------------------

module Graphics.Hexif.Utils where

import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)
import Data.List (intersperse)

-- little support functions: normal pack/unpack are refused by GHC
-- Hoogle says: unpack :: BL.ByteString -> String
-- GHCi says:   unpack ::  BL.ByteString -> [Word8]
-- In reality we converts 8-bit bytes to identical 8-bit bytes!
-- Why is Haskell string conversion so difficult?
unpackLazyBS :: BL.ByteString -> String
unpackLazyBS = map (chr . fromIntegral)  . BL.unpack

-- |Given a delimiter and a list of items (or strings), join the items
-- by using the delimiter.
-- From Data.List.Utils.
-- I dont' want to depend on all the dependecies of Data.List.Utils.
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)
