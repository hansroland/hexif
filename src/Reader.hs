-- -----------------------------------------------------------------------------
-- Reader.hs Functions to read in an Exif structure
-- -----------------------------------------------------------------------------

module Reader (readExif) where

import DataExif
import Utils

import Data.Char (chr)

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

type Offset = Int

readExif :: BL.ByteString -> [IFDFileDir]
readExif = runGet getExif 

-- Main function to read in the exif data structure
getExif :: Get [IFDFileDir]
getExif = do
    -- Tiff Header 
    tiffAlign <- getWord16be
    let getWords = if fromIntegral tiffAlign == 0x4949
        then (getWord16le, getWord32le)
        else (getWord16be, getWord32be) 
    const2A <- getByteString 2
    -- Get main directory
    wOffsetMain <- snd getWords
    mainBlock <- getIFDBlock (fromIntegral wOffsetMain) getWords
    let mainDir = IFDFileDir IFDMain getWords mainBlock
    -- Get sub directory
    wOffsetSub <- snd getWords
    let nOffsetSub = fromIntegral wOffsetSub
    if fromIntegral nOffsetSub == 0 
       then return [mainDir]
       else do
           subBlock <- getIFDBlock nOffsetSub getWords
           let subDir = IFDFileDir IFDExif getWords subBlock
           return [mainDir, subDir]

-- read a single IFD block. It contains n IFD entries. 
getIFDBlock :: Offset -> GetWords -> Get [IFDFileEntry]
getIFDBlock n getWords =
    if n == 0 
        then return []
        else do
           entry <- getIFDFileEntry getWords
           entries <- getIFDBlock (n - 1) getWords
           return $ entry : entries

-- read a single IFD file entry
getIFDFileEntry ::  GetWords -> Get IFDFileEntry
getIFDFileEntry (getWord16, getWord32)  = do
    tagNr <- getWord16
    format <- getWord16
    comps <- getWord32
    strBsValue <- getLazyByteString 4
    return $ IFDFileEntry tagNr format (fromIntegral comps) strBsValue

{-
-- convert IFDFileDir to IFDDir
convertDir :: IFDFileDir -> IFDDir
convertDir (IFDFileDir dirTag getWords fileEntries) =  
      IFDDir dirTag (map conf fileEntries)
         where conf = 
-}


-- convert a single IFDFile entry to an IFDEntry
convertEntry :: BL.ByteString -> GetWords -> IFDFileEntry -> IFDEntry
convertEntry bsExif words@(getWord16,getWord32)  (IFDFileEntry tag format len strBsValue) = 
   case format of
       0x0002 -> IFDStr exifTag (getStringValue len offsetOrValue bsExif)
       0x0003 -> IFDNum exifTag offsetOrValue 
       0x0004 -> IFDNum exifTag offsetOrValue
       0x0005 -> IFDRat exifTag (getRationalValue offsetOrValue bsExif words)
       0x0007 -> IFDUdf exifTag len (unpackLazyBS bsExif)
       0x000A -> IFDRat exifTag (getRationalValue offsetOrValue bsExif words)
       _      -> error $ "Format " ++ show format ++ " not yet implemented"  
   where 
      exifTag = toExifTag tag
      offsetOrValue = fromIntegral (runGet getWord32 strBsValue)
      -- formats
      -- 0x0002 = ascii string
      -- 0x0003 = unsigned short
      -- 0x0004 = unsigned long
      -- 0x0005 = unsigned rational
      -- 0x0007 = undefined
      -- 0x000A = signed rationale

-- subfunctions of convert   
getStringValue :: Int -> Int -> BL.ByteString -> String
getStringValue len offset = runGet (getString len offset)
    where      
        getString :: Int -> Int -> Get String
        getString len offset = do
            skip offset
            lazy <- getLazyByteString $ fromIntegral (len - 1) 
            return $ unpackLazyBS lazy

getRationalValue :: Int -> BL.ByteString -> GetWords -> (Int, Int)
getRationalValue offset bsExif words = runGet (getRationale words offset) bsExif
    where
        getRationale :: GetWords -> Int -> Get (Int, Int)
        getRationale words@(getWord16, getWord32) offset = do
            skip offset
            num <- getWord32
            denum <- getWord32
            return (fromIntegral num, fromIntegral denum)

-- Convert a Word16 Number to an Exif Tag
toExifTag :: Word16 -> ExifTag
toExifTag t 
   | t == 0x0103 = TagCompression
   | t == 0x010e = TagImageDescription
   | t == 0x010f = TagMake
   | t == 0x0110 = TagModel
   | t == 0x0112 = TagOrientation
   | t == 0x011a = TagXResolution
   | t == 0x011b = TagYResolution
   | t == 0x0128 = TagResolutionUnit
   | t == 0x0132 = TagDateTime
   | t == 0x0201 = TagJPEGInterchangeFormat
   | t == 0x0202 = TagJPEGInterchangeFormatLength
   | t == 0x0213 = TagYCbCrPositioning
   | t == 0x829a = TagExposureTime
   | t == 0x829d = TagFNumber
   | t == 0x8822 = TagExposureProgram
   | t == 0x8827 = TagISOSpeedRatings
   | t == 0x9000 = TagExifVersion
   | t == 0x9003 = TagDateTimeOriginal
   | t == 0x9004 = TagDateTimeDigitized
   | t == 0x9101 = TagComponentsConfiguration
   | t == 0x9102 = TagCompressedBitsPerPixel
   | t == 0x9204 = TagExposureBiasValue
   | t == 0x9205 = TagMaxApertureValue
   | t == 0x9207 = TagMeteringMode
   | t == 0x9208 = TagLightSource
   | t == 0x9209 = TagFlash
   | t == 0x920a = TagFocalLength
   | t == 0x927c = TagMakerNote
   | t == 0xa000 = TagFlashPixVersion
   | t == 0xa001 = TagColorSpace
   | t == 0xa002 = TagPixelXDimension
   | t == 0xa003 = TagPixelYDimension
   | t == 0xa300 = TagFileSource
   | t == 0xa301 = TagSceneType
   | t == 0xa401 = TagCustomRendered
   | t == 0xa402 = TagExposureMode
   | t == 0xa403 = TagWhiteBalance
   | t == 0xa406 = TagSceneCaptureType
   | t == 0xc4a5 = TagPrintImageMatching
   | otherwise = TagTagUnknown t

