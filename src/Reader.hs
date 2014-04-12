-- -----------------------------------------------------------------------------
-- Reader.hs Functions to read in an Exif structure
-- -----------------------------------------------------------------------------

module Reader where

import DataExif
import Utils

import Data.Char (chr)
import Data.List (partition)

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

type Offset = Int

readExif :: BL.ByteString -> Exif
readExif input = Exif mainDirs getWords 
    where 
      mainDirs = map (convertDir input getWords) mainFileDirs
      mainFileDirs = readIFDFileDirs offset getWords input
      (getWords, offset) = readHeader input

-- Read the header data from an EXIF file 
readHeader :: BL.ByteString -> (GetWords, Offset)
readHeader = runGet getHeader
  where 
    getHeader :: Get (GetWords, Offset)
    getHeader = do
        -- Tiff Header 
        tiffAlign <- getWord16be
        let getWords = if fromIntegral tiffAlign == 0x4949
            then (getWord16le, getWord32le)
            else (getWord16be, getWord32be) 
        const2A <- getByteString 2
        -- Get offset to main directory
        offset <- fst getWords
        return (getWords, fromIntegral offset)


-- read chained FileDirs from a given offset
readIFDFileDirs :: Offset -> GetWords -> BL.ByteString -> [IFDFileDir]
readIFDFileDirs offset getWords = runGet (getIFDBlocks offset getWords)
  where
    getIFDFileDirs :: Offset -> GetWords -> Get [IFDFileDir]
    getIFDFileDirs offset getWords@(getWord16, getWord32) = do
        skip offset
        count <- getWord16
        getIFDBlocks (fromIntegral count) getWords

-- read chained IFD blocks
getIFDBlocks :: Int -> GetWords -> Get [IFDFileDir]
getIFDBlocks nOffset getWords@(getWord16, getWord32) = do
    if nOffset == 0
        then return []
        else do 
           skip nOffset
           -- pos <- bytesRead
           -- skip $  nOffset - (fromIntegral pos)
           nEntries <- getWord16
           block <- getIFDBlock (fromIntegral nEntries) getWords
           -- next <- getWord32
           -- blocks <- getIFDBlocks (fromIntegral next) getWords
           -- return $ block : blocks
           return [block]

-- read a single IFD block. It contains n IFD entries. 
getIFDBlock :: Int -> GetWords -> Get IFDFileDir
getIFDBlock count getWords =
    if count == 0 
        then return []
        else do
           entry <- getIFDFileEntry getWords
           entries <- getIFDBlock (count - 1) getWords
           return $ entry : entries

-- read a single IFD file entry
getIFDFileEntry ::  GetWords -> Get IFDFileEntry
getIFDFileEntry (getWord16, getWord32)  = do
    tagNr <- getWord16
    format <- getWord16
    comps <- getWord32
    strBsValue <- getLazyByteString 4
    return $ IFDFileEntry tagNr format (fromIntegral comps) strBsValue

-- convert IFDFileDir to IFDDir
convertDir :: BL.ByteString -> GetWords -> IFDFileDir -> IFDDir
convertDir bsExif getWords fileEntries = map conf fileEntries
  where 
    conf = convertEntry bsExif getWords
 
-- convert a single IFDFile entry to an IFDEntry
convertEntry :: BL.ByteString -> GetWords -> IFDFileEntry -> IFDEntry
convertEntry bsExif getWords  fileEntry@(IFDFileEntry tag format len strBsValue) = 
   case toTag of
       Just dirTag -> convertSubEntry dirTag bsExif getWords fileEntry
       Nothing     -> convertStdEntry bsExif getWords fileEntry
   where toTag = toDirTag tag 

convertSubEntry :: DirTag -> BL.ByteString -> GetWords -> IFDFileEntry -> IFDEntry
convertSubEntry dirTag bsExif getWords@(getWord16,getWord32) (IFDFileEntry tag format len strBsValue) = IFDSub  dirTag subDir
    where 
      subDir = convertDir bsExif getWords fileDir 
      fileDir = concat $ readIFDFileDirs offset getWords bsExif
      offset = fromIntegral (runGet getWord32 strBsValue)

convertStdEntry :: BL.ByteString -> GetWords -> IFDFileEntry -> IFDEntry
convertStdEntry bsExif words@(getWord16,getWord32)  (IFDFileEntry tag format len strBsValue) = 
   case format of
       0x0002 -> IFDStr exifTag (stringValue tag len strBsValue getWord32 bsExif)
       0x0003 -> IFDNum exifTag offsetOrValue 
       0x0004 -> IFDNum exifTag offsetOrValue
       0x0005 -> IFDRat exifTag (rationalValue offsetOrValue bsExif words)
       0x0007 -> IFDUdf exifTag len (unpackLazyBS strBsValue)
       0x000A -> IFDRat exifTag (rationalValue offsetOrValue bsExif words)
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
-- Note: TagInteroperabilityIndex has a non standard representation
stringValue :: Word16 -> Int -> BL.ByteString -> Get Word32 -> BL.ByteString -> String
stringValue  1  len strBsValue _  _       = unpackLazyBS strBsValue
stringValue tag len strBsValue getWord32 bsExif = runGet (getStringValue len offset) bsExif
    where    
        offset = fromIntegral (runGet getWord32 strBsValue)  
        getStringValue :: Int -> Int -> Get String
        getStringValue len offset = do
            skip offset
            lazy <- getLazyByteString $ fromIntegral (len - 1) 
            return $ unpackLazyBS lazy

rationalValue :: Int -> BL.ByteString -> GetWords -> (Int, Int)
rationalValue offset bsExif words = runGet (getRationaleValue words offset) bsExif
    where
        getRationaleValue :: GetWords -> Int -> Get (Int, Int)
        getRationaleValue words@(getWord16, getWord32) offset = do
            skip offset
            num <- getWord32
            denum <- getWord32
            return (fromIntegral num, fromIntegral denum)

-- Convert a Word16 number to an Maybe DirTag
toDirTag :: Word16 -> Maybe DirTag
toDirTag t 
    | t == 0x8769 = Just IFDExif
    | t == 0xa005 = Just IFDInterop
    | t == 0x8825 = Just IFDGPS
	| otherwise   = Nothing

-- Convert a Word16 number to an Exif Tag
toExifTag :: Word16 -> ExifTag
toExifTag t 
   | t == 0x0001 = TagInteroperabilityIndex
   | t == 0x0002 = TagInteroperabilityVersion
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

