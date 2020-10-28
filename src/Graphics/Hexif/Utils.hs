module Graphics.Hexif.Utils

where


import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString.Lazy as BL

import Data.Hex

--
-- | The encoding of the binary data.
-- Motorola is big endian, Intel is low endian
data Encoding = Intel
    | Motorola
  deriving (Show)



-- | Little support function to read 16 bit integers
getWord16 :: Encoding -> Get Word16
getWord16 Motorola = getWord16be
getWord16 Intel = getWord16le

-- | Little support function to read 32 bit integers
getWord32 :: Encoding -> Get Word32
getWord32 Motorola = getWord32be
getWord32 Intel = getWord32le

-- | Convert a lazy ByteString into a normal Haskell String
-- Copied from MissingH library module Data.Bits.Utils
unpackLazyBS :: BL.ByteString -> String
unpackLazyBS = map (toEnum . fromIntegral)  . BL.unpack

showHex :: Binary a => a -> String
showHex = show . hex . encode

runGetEither :: Get a -> BL.ByteString -> Either String a
runGetEither get bs = case runGetOrFail get bs of
    Left (_,_,strError) -> Left strError
    Right (_,_,x)       -> Right x
