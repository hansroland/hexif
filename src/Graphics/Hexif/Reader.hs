-- -----------------------------------------------------------------------------
-- Reader.hs Functions to read in an Exif structure
-- -----------------------------------------------------------------------------

module Graphics.Hexif.Reader where

import Graphics.Hexif.DataExif
import Graphics.Hexif.Utils

import Data.Char (chr, ord)
import Data.List (partition)

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

type Offset = Int

-- read in an Exif file.
-- The ByteString parameter starts with the EXIF__ constant
readExif :: BL.ByteString -> Exif
readExif bsExif6 = Exif mainDirs getWords 
    where 
      bsExif = BL.drop 6 bsExif6
      mainDirs = map (convertDir IFDMain bsExif getWords) mainFileDirs
      mainFileDirs = readIFDFileDirs IFDMain offset getWords bsExif
      (getWords, offset) = readHeader bsExif

-- Read the header data from a ByteString representing an EXIF file 
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
        offset <- snd getWords
        return (getWords, fromIntegral offset)

-- read chained FileDirs from a given offset
readIFDFileDirs :: DirTag -> Offset -> GetWords -> BL.ByteString -> [IFDFileDir]
readIFDFileDirs dirTag offset getWords bsExif = 
  if offset == 0
     then []
     else (debug :[]) : block : blocks
     -- else block : blocks       -- without debugging
  where
    (next, block) = readIFDFileDir offset getWords bsExif
    blocks = readIFDFileDirs dirTag next getWords bsExif
    debug = IFDFileEntry (dirTagToWord16 dirTag) 0 offset (BL.pack [])                            -- debug entry
    
-- read a single IFD File Directory from a given offset
readIFDFileDir :: Offset -> GetWords -> BL.ByteString -> (Offset, IFDFileDir)
readIFDFileDir offset getWords = runGet (getIFDFileDir offset getWords) 


-- get IFD directory and the offset pointing to the next chained IFD 
getIFDFileDir :: Int -> GetWords -> Get (Offset, IFDFileDir)
getIFDFileDir nOffset getWords@(getWord16, getWord32) = do
           skip nOffset
           nEntries <- getWord16
           block <- getIFDFileDirEntries (fromIntegral nEntries) getWords
           next <- getWord32
           return (fromIntegral next, block)

-- Get all the entries of an IFD File Dir
getIFDFileDirEntries :: Int -> GetWords -> Get IFDFileDir
getIFDFileDirEntries count getWords =
    if count == 0 
        then return []
        else do
           entry <- getIFDFileEntry getWords
           entries <- getIFDFileDirEntries (count - 1) getWords
           return $ entry : entries

-- Get a single IFD file entry
getIFDFileEntry ::  GetWords -> Get IFDFileEntry
getIFDFileEntry (getWord16, getWord32)  = do
    tagNr <- getWord16
    format <- getWord16
    comps <- getWord32
    strBsValue <- getLazyByteString 4
    return $ IFDFileEntry tagNr format (fromIntegral comps) strBsValue

-- convert IFDFileDir to IFDDir
convertDir :: DirTag -> BL.ByteString -> GetWords -> IFDFileDir -> IFDDir
convertDir dirTag bsExif getWords fileEntries = map conf fileEntries
  where 
    conf = convertEntry dirTag bsExif getWords
 
-- convert a single IFDFile entry to an IFDEntry
convertEntry :: DirTag -> BL.ByteString -> GetWords -> IFDFileEntry -> IFDEntry
convertEntry dirTag bsExif getWords  fileEntry@(IFDFileEntry tag format len strBsValue) = 
   case toTag of
       Just dirTag -> convertSubEntry dirTag bsExif getWords fileEntry
       Nothing     -> convertStdEntry dirTag bsExif getWords fileEntry
   where toTag = toDirTag tag 

-- a sub entry contains a new IFD File directory
convertSubEntry :: DirTag -> BL.ByteString -> GetWords -> IFDFileEntry -> IFDEntry
convertSubEntry dirTag bsExif getWords@(getWord16,getWord32) (IFDFileEntry tag format len strBsValue) = IFDSub  dirTag subDir
    where 
      subDir = convertDir dirTag bsExif getWords fileDir 
      fileDir = concat $ readIFDFileDirs dirTag offset getWords bsExif
      offset = fromIntegral (runGet getWord32 strBsValue)

-- a standard entry represents a single tag and its value
convertStdEntry :: DirTag -> BL.ByteString -> GetWords -> IFDFileEntry -> IFDEntry
convertStdEntry dirTag bsExif words@(getWord16,getWord32)  (IFDFileEntry tag format len strBsValue) = 
   case format of
       0  -> IFDNum exifTag len                                                  -- debug entry
       1  -> IFDNum exifTag byteValue 
       2  -> IFDStr exifTag (stringValue dirTag exifTag len strBsValue getWord32 bsExif)
       3  -> IFDNum exifTag offsetOrValue16 
       4  -> IFDNum exifTag offsetOrValue32
       5  -> IFDRat exifTag (rationalValue offsetOrValue32 bsExif words)
       7  -> IFDUdf exifTag len (unpackLazyBS strBsValue)
       10 -> IFDRat exifTag (rationalValue offsetOrValue32 bsExif words)
       _      -> error $ "Format " ++ show format ++ " not yet implemented"  
   where 
      exifTag = toExifTag dirTag tag
      offsetOrValue32 = fromIntegral (runGet getWord32 strBsValue)
      offsetOrValue16 = fromIntegral (runGet getWord16 strBsValue)
      byteValue = fromIntegral (runGet getWord8 strBsValue)
      -- formats
      -- 0x0001 = unsigned byte
      -- 0x0002 = ascii string
      -- 0x0003 = unsigned short
      -- 0x0004 = unsigned long
      -- 0x0005 = unsigned rational
      -- 0x0007 = undefined
      -- 0x000A = signed rationale

-- subfunctions of convert

-- read out a string value 
-- Note: Some tags have non standard representation -> Special cases
stringValue :: DirTag -> ExifTag -> Int -> BL.ByteString -> Get Word32 -> BL.ByteString -> String
stringValue IFDGPS TagGPSLatitudeRef len strBsValue         _  _       = take 1 (unpackLazyBS strBsValue)
stringValue IFDGPS TagGPSLongitudeRef len strBsValue        _  _       = take 1 (unpackLazyBS strBsValue)
stringValue dirTag TagInteroperabilityIndex  len strBsValue _  _       = take 3 (unpackLazyBS strBsValue)
stringValue dirTag _ len strBsValue getWord32 bsExif = clean $ runGet (getStringValue len offset) bsExif
    where
        offset = fromIntegral (runGet getWord32 strBsValue)
        clean = reverse . dropWhile (\c -> ord c == 0) . reverse
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

dirTagToWord16 :: DirTag -> Word16
dirTagToWord16 IFDMain = 0xFF01 
dirTagToWord16 IFDExif = 0xFF02
dirTagToWord16 IFDInterop = 0xFF03
dirTagToWord16 IFDGPS = 0xFF04

-- Convert a Word16 number to an Exif Tag
toExifTag :: DirTag -> Word16 -> ExifTag
toExifTag IFDGPS tag = toGPSTag tag
toExifTag _      tag = toStdTag tag

toStdTag :: Word16 -> ExifTag
toStdTag t = case t of
   0x0001 -> TagInteroperabilityIndex
   0x0002 -> TagInteroperabilityVersion
   0x0100 -> TagImageWidth
   0x0101 -> TagImageLength
   0x0102 -> TagBitsPerSample
   0x0103 -> TagCompression
   0x010e -> TagImageDescription
   0x010f -> TagMake
   0x0110 -> TagModel
   0x0112 -> TagOrientation
   0x011a -> TagXResolution
   0x011b -> TagYResolution
   0x0128 -> TagResolutionUnit
   0x0131 -> TagSoftware
   0x0132 -> TagDateTime
   0x013b -> TagArtist
   0x0201 -> TagJPEGInterchangeFormat
   0x0202 -> TagJPEGInterchangeFormatLength
   0x0213 -> TagYCbCrPositioning
   0x1001 -> TagRelatedImageWidth
   0x1002 -> TagRelatedImageLength
   0x8298 -> TagCopyright
   0x829a -> TagExposureTime
   0x829d -> TagFNumber
   0x8822 -> TagExposureProgram
   0x8827 -> TagISOSpeedRatings
   0x9000 -> TagExifVersion
   0x9003 -> TagDateTimeOriginal
   0x9004 -> TagDateTimeDigitized
   0x9101 -> TagComponentsConfiguration
   0x9102 -> TagCompressedBitsPerPixel
   0x9201 -> TagShutterSpeedValue
   0x9202 -> TagApertureValue
   0x9203 -> TagBrightnessValue
   0x9204 -> TagExposureBiasValue
   0x9205 -> TagMaxApertureValue
   0x9207 -> TagMeteringMode
   0x9208 -> TagLightSource
   0x9209 -> TagFlash
   0x920a -> TagFocalLength
   0x927c -> TagMakerNote
   0x9286 -> TagUserComment
   0xa000 -> TagFlashPixVersion
   0xa001 -> TagColorSpace
   0xa002 -> TagPixelXDimension
   0xa003 -> TagPixelYDimension
   0xa20e -> TagFocalPlaneXResolution
   0xa20f -> TagFocalPlaneYResolution
   0xa210 -> TagFocalPlaneResolutionUnit
   0xa217 -> TagSensingMethod
   0xa300 -> TagFileSource
   0xa301 -> TagSceneType
   0xa401 -> TagCustomRendered
   0xa402 -> TagExposureMode
   0xa403 -> TagWhiteBalance
   0xa404 -> TagDigitalZoomRatio
   0xa405 -> TagFocalLengthIn35mmFilm
   0xa406 -> TagSceneCaptureType
   0xa407 -> TagGainControl
   0xa408 -> TagContrast
   0xa409 -> TagSaturation
   0xa40a -> TagSharpness
   0xa40c -> TagSubjectDistanceRange
   0xa420 -> TagImageUniqueID
   0xc4a5 -> TagPrintImageMatching
   0xc6d2 -> TagPanasonicTitle1
   0xc6d3 -> TagPanasonicTitle2
   0xea1c -> TagPadding
   0xff01 -> TagSubDir_IFDMain
   0xff02 -> TagSubDir_IFDExif
   0xff03 -> TagSubDir_IFDInterop
   _ -> TagTagUnknown t


toGPSTag :: Word16 -> ExifTag
toGPSTag t = case t of
   0x0000 -> TagGPSVersionID
   0x0001 -> TagGPSLatitudeRef
   0x0002 -> TagGPSLatitude
   0x0003 -> TagGPSLongitudeRef
   0x0004 -> TagGPSLogitude
   0x0005 -> TagGPSAltitudeRef
   0x0006 -> TagGPSAltitude 
   0x0007 -> TagGPSTimeStamp
   0x0012 -> TagGPSMapDatum
   0x001d -> TagGPSDateStamp
   0xff04 -> TagSubDir_IFDGPS
   _ -> TagTagUnknown t


