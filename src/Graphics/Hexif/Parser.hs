module Graphics.Hexif.Parser
    (parseRawExif
    )
where

import Graphics.Hexif.Types
import Graphics.Hexif.Utils

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Binary.Get
import Data.Binary
import Control.Monad

import Data.Maybe

-- | Parse a bytestring into an Exif RawIfd structure
parseRawExif :: BL.ByteString -> Exif RawIfd
parseRawExif bs = Exif
      { encoding = enc
      , ifdMap = ifdMap
      }
    where
      (enc, offset0) = parseHeader bs
      (offset1, mainIfd) = parseRawIfd enc bs offset0

      mb1stIfd = if offset1 == 0
                    then Nothing
                    else Just $ snd $ parseRawIfd enc bs offset1

      mbExifIfd = (findRawEntryOffset mainIfd tagExifIfd)
                >>= mbParseRawIfd enc bs
      mbGpsIfd  = (findRawEntryOffset mainIfd tagGpsIfd)
                >>= mbParseRawIfd enc bs
      mbInterIfd = case mbExifIfd of
                    Nothing -> Nothing
                    Just inter ->  (findRawEntryOffset inter tagIntopIfd)
                       >>= mbParseRawIfd enc bs

      mbPair a mbB = case mbB of
        Nothing -> Nothing
        Just b  -> Just (a, b)

      ifdMap = Map.fromList $ catMaybes $
        [ mbPair tag0thIfd (Just mainIfd)
        , mbPair tagExifIfd mbExifIfd
        , mbPair tagIntopIfd mbInterIfd
        , mbPair tagGpsIfd mbGpsIfd
        , mbPair tag1stIfd mb1stIfd
        ]

      findRawEntryOffset :: RawIfd -> Word16 -> Maybe Word32
      findRawEntryOffset ifd rawTag = rawValue32 <$> Map.lookup rawTag ifd

-- | Read the header data from a ByteString representing an EXIF file
parseHeader :: BL.ByteString -> (Encoding, Word32)
parseHeader = runGet getHeader
  where
    getHeader :: Get (Encoding, Word32)
    getHeader = do
        -- Tiff Header
        tiffAlign <- getWord16be
        let encoding = if fromIntegral tiffAlign == (0x4949::Int)
            then Intel
            else Motorola
        _ <- getByteString 2                     -- const x'2A' = 42 !!
        -- Get offset to main directory
        offset <- getWord32 encoding
        return (encoding, offset)

-- Parse an Ifd from the bytestring,
-- Return the offset to the next Ifd and the parsed IFD
parseRawIfd :: Encoding ->  BL.ByteString -> Word32 -> (Word32, RawIfd)
parseRawIfd enc bs offset = runGet (getRawIfd enc) bs
  where
    getRawIfd :: Encoding -> Get (Word32, RawIfd)
    getRawIfd enc = do
        skip $ fromIntegral offset
        n <- getWord16 enc
        raws <- replicateM (fromIntegral n) (parseRawEntry enc)
        let rawPairs = map (\e -> (rawEntryTag e, e)) raws
        nextIfd <- getWord32 enc
        return $ (nextIfd, Map.fromList rawPairs)

    parseRawEntry :: Encoding -> Get RawEntry
    parseRawEntry enc = do
      tag <- getWord16 enc
      fmt <- getWord16 enc
      len <- getWord32 enc
      bsValue <- getLazyByteString 4
      let val16 = runGet (getWord16 enc) bsValue
      let val32 = runGet (getWord32 enc) bsValue
      let str   = unpackLazyBS bsValue
      return $ RawEntry tag fmt len val16 val32 str

mbParseRawIfd :: Encoding ->  BL.ByteString ->  Word32 -> Maybe RawIfd
mbParseRawIfd enc bs offset = Just $ snd $ parseRawIfd enc bs offset
