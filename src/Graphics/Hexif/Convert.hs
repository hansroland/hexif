module Graphics.Hexif.Convert (convert)

  where

import Graphics.Hexif.Utils
import Graphics.Hexif.Types

import Control.Monad
import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

-- Convert an Ifd with raw values to one with converted values
convert :: BL.ByteString -> Exif RawIfd -> Exif Ifd
convert bsExif (Exif enc ifd) = Exif enc newIfd
    where
      newIfd = Map.mapMaybeWithKey mapper ifd
      mapper :: Word16 -> RawIfd -> Maybe Ifd
      mapper key rawIfd = convertIfd bsExif enc (getToTags key) rawIfd

      getToTags :: Word16 -> Maybe (Word16 -> ExifTag)
      getToTags key = Map.lookup key tagsMap

convertIfd :: BL.ByteString -> Encoding -> Maybe (Word16 -> ExifTag) -> RawIfd -> Maybe Ifd
convertIfd _ _ Nothing _ = Nothing
convertIfd bsexif enc (Just toTag) ifd = Just dict
  where
    dict = Map.fromList keyvals
    keyvals = map (\e -> (entryTag e, e)) entries
    entries = map (convertEntry bsexif enc toTag) remIfd
    -- remove the ifd pointers
    remIfd = filter (\e -> rawEntryTag e  `notElem`  ifdTags ) (Map.elems ifd)


convertEntry :: BL.ByteString -> Encoding -> (Word16 -> ExifTag) ->
                RawEntry -> IfdEntry
convertEntry bsExif enc toTag (RawEntry  entryTag dataType  count rawVal16 rawVal32 rawStr) =
    IfdEntry exifTag val
  where
    val = case dataType of
         0  -> ValueInt len
         1  -> ValueStr $ take len $ (concatMap (show . fromEnum)) rawStr
         2  -> ValueStr (stringValue exifTag len value32 rawStr bsExif)
         3  -> ValueInt value16
         4  -> ValueInt value32
         5  -> ValueRat (rationalValues len value32 bsExif enc)
         7  -> ValueUdf len rawStr
         9  -> ValueInt value32
         10 -> ValueRat (rationalValues len value32 bsExif enc)
         _      -> error $ "Format " ++ show len ++ " not yet implemented"

    exifTag = toTag entryTag
    len = fromIntegral count
    value16 = fromIntegral rawVal16
    value32 = fromIntegral rawVal32

-- | Read out a string value.
-- Note: Some tags have non standard representation -> Special cases
stringValue :: ExifTag -> Int -> Int -> String -> BL.ByteString -> String
stringValue TagSubsecTime          len _ strVal  _    = take len strVal
stringValue TagSubSecTimeOriginal  len _ strVal  _    = take len strVal
stringValue TagSubSecTimeDigitized len _ strVal  _    = take len strVal
stringValue TagGPSLatitudeRef      _   _ strVal  _    = directByte strVal
stringValue TagGPSLongitudeRef     _   _ strVal  _    = directByte strVal
stringValue TagGPSDestLatitudeRef  _   _ strVal  _    = directByte strVal
stringValue TagGPSDestLongitudeRef _   _ strVal  _    = directByte strVal
stringValue TagGPSImgDirectionRef  _   _ strVal  _    = directByte strVal
stringValue TagGPSSpeedRef         _  _ strVal   _    =  directByte strVal
stringValue TagGPSDestBearingRef   _  _ strVal   _    =  directByte strVal

stringValue TagInteroperabilityIndex  _ _ strVal _    = take 3 strVal
-- TODO use 'runGetEither' from the Utils module!!!
stringValue _ len offsetOrValue _ bsExif = runGet getStringValue bsExif
    where
        getStringValue = do
            skip offsetOrValue
            lazy <- getLazyByteString $ fromIntegral (len - 1)
            return $ unpackLazyBS lazy

-- | Fetch a direct byte
directByte :: String -> String
directByte = take 1

-- | Read the rational values of on exif tag
rationalValues :: Int -> Int -> BL.ByteString -> Encoding -> [(Int, Int)]
rationalValues comps offset bsExif encoding = runGet getRationalValues bsExif
    where
        getRationalValues :: Get [(Int,Int)]
        getRationalValues = do
            skip offset
            replicateM comps getRationalValue
        getRationalValue :: Get (Int, Int)
        getRationalValue = do
            num <- getWord32 encoding
            denum <- getWord32 encoding
            return (fromIntegral num, fromIntegral denum)
