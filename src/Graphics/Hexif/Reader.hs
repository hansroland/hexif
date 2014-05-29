-- | This module contains the code to read (or parse) the exif file.
-- This module is an internal module of Graphics.Hexif and should only be used in the hexif project!

module Graphics.Hexif.Reader where

import Graphics.Hexif.DataExif
import Graphics.Hexif.Utils

import Data.Char (chr)
import Control.Monad(replicateM)

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

-- | Offset into the lazy ByteString
type Offset = Int

-- | A list of file entries builds a directory.
type IFDDir = [IFDEntry]
     
-- | Representation of a physical IFD Entry in the exif file
data IFDEntry = IFDEntry Word16 Word16 Int BL.ByteString
    | IFDSubDir DirTag IFDDir
    deriving (Eq, Show)

-- | Read whole Exif file into our Haskell exif value.
-- The ByteString parameter starts with the EXIF__ constant.
readExif :: BL.ByteString -> Exif
readExif bsExif6 = Exif mainDirs getWords 
    where 
      bsExif = BL.drop 6 bsExif6
      mainDirs = map (convertDir IFDMain bsExif getWords) ifdDirs
      ifdDirs = readIFDDirs IFDMain offset getWords bsExif
      (getWords, offset) = readHeader bsExif

-- | Read the header data from a ByteString representing an EXIF file 
readHeader :: BL.ByteString -> (GetWords, Offset)
readHeader = runGet getHeader
  where 
    getHeader :: Get (GetWords, Offset)
    getHeader = do
        -- Tiff Header 
        tiffAlign <- getWord16be
        let getWords = if fromIntegral tiffAlign == (0x4949::Int)
            then (getWord16le, getWord32le)
            else (getWord16be, getWord32be) 
        _ <- getByteString 2						-- const 2A
        -- Get offset to main directory
        offset <- snd getWords
        return (getWords, fromIntegral offset)

-- | Read chained IFD Dirs from a given offset.
readIFDDirs :: DirTag -> Offset -> GetWords -> BL.ByteString -> [IFDDir]
readIFDDirs dirTag offset getWords bsExif = 
  if offset == 0
     then []
     else [debug] : block : blocks
     -- else block : blocks       -- without debugging
  where
    (next, block) = readIFDDir offset getWords bsExif
    blocks = readIFDDirs dirTag next getWords bsExif
    debug = IFDEntry (dirTagToWord16 dirTag) 0 offset (BL.pack [])  -- debug entry
    
-- | Read a single IFD Directory from a given offset
readIFDDir :: Offset -> GetWords -> BL.ByteString -> (Offset, IFDDir)
readIFDDir offset getWords@(getWord16, getWord32) bsExif = runGet getIFDDir bsExif
  where
    -- Get IFD directory and the offset pointing to the next chained IFD.
    getIFDDir :: Get (Offset, IFDDir)
    getIFDDir = do
        skip offset
        nEntries <- getWord16
        block <- getIFDDirEntries (fromIntegral nEntries) getWords bsExif
        next <- getWord32
        return (fromIntegral next, block)

-- | Get all the entries of an IFD
getIFDDirEntries :: Int -> GetWords -> BL.ByteString -> Get IFDDir
getIFDDirEntries count getWords@(getWord16, getWord32) bsExif =
    if count == 0
        then return []
        else do
           entry <- getIFDEntry
           entries <- getIFDDirEntries (count - 1) getWords bsExif
           return $ entry : entries
    where
      -- Get a single IFD entry.
      getIFDEntry = do
          tag <- getWord16
          fmt <- getWord16
          comps <- getWord32
          strBsValue <- getLazyByteString 4
          return $ buildEntry tag fmt comps strBsValue
      -- Read subdirectories here 
      buildEntry tg fm cmps strBsVal = 
          case toDirTag tg of
              Just dirTag  -> IFDSubDir dirTag (concat $ readIFDDirs dirTag offset getWords bsExif)
              Nothing  -> IFDEntry tg fm (fromIntegral cmps) strBsVal
            where
               offset = fromIntegral (runGet getWord32 strBsVal)

-- | Convert IFD Entries to DataEntries
convertDir :: DirTag -> BL.ByteString -> GetWords -> IFDDir -> DataBlock
convertDir dirTag bsExif getWords = map (convertEntry dirTag bsExif getWords)

-- | Convert a single IFDEntry
convertEntry :: DirTag -> BL.ByteString -> GetWords -> IFDEntry -> DataEntry
convertEntry _ bsExif getWords (IFDSubDir dTg ifdDir) = DataSub dTg (convertDir dTg bsExif getWords ifdDir)
convertEntry dirTag bsExif getWords@(getWord16, getWord32) (IFDEntry tg fmt len strBsValue) =
   case fmt of
         0  -> DataNum exifTag Fmt00 len                                                  -- debug entry
         1  -> DataStr exifTag Fmt01 (byteValues exifTag offsetOrValue32 len)
         2  -> DataStr exifTag Fmt02 (stringValue exifTag len strBsValue getWord32 bsExif)
         3  -> DataNum exifTag Fmt03 offsetOrValue16
         4  -> DataNum exifTag Fmt04 offsetOrValue32
         5  -> DataRat exifTag Fmt05 (rationalValues len offsetOrValue32 bsExif getWords)
         7  -> DataUdf exifTag Fmt07 len (unpackLazyBS strBsValue)
         9  -> DataNum exifTag Fmt09 offsetOrValue32
         10 -> DataRat exifTag Fmt10 (rationalValues len offsetOrValue32 bsExif getWords)
         _      -> error $ "Format " ++ show fmt ++ " not yet implemented"
     where
       exifTag = toExifTag dirTag tg
       offsetOrValue32 = fromIntegral (runGet getWord32 strBsValue)
       offsetOrValue16 = fromIntegral (runGet getWord16 strBsValue)
       byteValues TagGPSVersionID   _ _ = concatMap show (BL.unpack strBsValue)
       byteValues TagGPSAltitudeRef _ _ = show $ head $ BL.unpack strBsValue
       byteValues _ offset ln =  map (chr . fromIntegral) $ runGet (skip offset >> replicateM ln getWord8)  bsExif
       -- formats
       -- 0x0001 = unsigned byte
       -- 0x0002 = ascii string
       -- 0x0003 = unsigned short
       -- 0x0004 = unsigned long
       -- 0x0005 = unsigned rational
       -- 0x0007 = undefined
       -- 0x0009 = signed long
       -- 0x000A = signed rationale


-- subfunctions of convert

-- | Read out a string value.
-- Note: Some tags have non standard representation -> Special cases
stringValue :: ExifTag -> Int -> BL.ByteString -> Get Word32 -> BL.ByteString -> String
stringValue TagSubsecTime len strBsValue          _  _       = take len (unpackLazyBS strBsValue)
stringValue TagSubSecTimeOriginal len strBsValue  _  _       = take len (unpackLazyBS strBsValue)
stringValue TagSubSecTimeDigitized len strBsValue  _  _      = take len (unpackLazyBS strBsValue)
stringValue TagGPSLatitudeRef _ strBsValue         _  _       = directByte strBsValue
stringValue TagGPSLongitudeRef _ strBsValue        _  _       = directByte strBsValue
stringValue TagGPSDestLatitudeRef _ strBsValue     _  _       = directByte strBsValue
stringValue TagGPSDestLongitudeRef _ strBsValue    _  _       = directByte strBsValue
stringValue TagGPSImgDirectionRef _ strBsValue     _  _       = directByte strBsValue
stringValue TagInteroperabilityIndex  _ strBsValue _  _     = take 3 (unpackLazyBS strBsValue)
stringValue _ len strBsValue getWord32 bsExif = runGet getStringValue bsExif
    where
        offset = fromIntegral (runGet getWord32 strBsValue)
        getStringValue = do
            skip offset
            lazy <- getLazyByteString $ fromIntegral (len - 1)
            return $ unpackLazyBS lazy

-- | Fetch a direct byte
directByte :: BL.ByteString -> String
directByte strBsValue = take 1 (unpackLazyBS strBsValue)

-- | Read the rational values of on exif tag
rationalValues :: Int -> Int -> BL.ByteString -> GetWords -> [(Int, Int)]
rationalValues comps offset bsExif (_, getWord32) = runGet getRationalValues bsExif
    where
        getRationalValues :: Get [(Int,Int)]
        getRationalValues = do
            skip offset
            replicateM comps getRationalValue
        getRationalValue :: Get (Int, Int)
        getRationalValue = do
            num <- getWord32
            denum <- getWord32
            return (fromIntegral num, fromIntegral denum)

-- | Convert a Word16 number to an Maybe DirTag
toDirTag :: Word16 -> Maybe DirTag
toDirTag t 
    | t == 0x8769 = Just IFDExif
    | t == 0xa005 = Just IFDInterop
    | t == 0x8825 = Just IFDGPS
	| otherwise   = Nothing

-- | Convert a directory tag to a number
dirTagToWord16 :: DirTag -> Word16
dirTagToWord16 IFDMain = 0xFF01 
dirTagToWord16 IFDExif = 0xFF02
dirTagToWord16 IFDInterop = 0xFF03
dirTagToWord16 IFDGPS = 0xFF04

-- | Convert a Word16 number together with its directory tag to an Exif Tag.
toExifTag :: DirTag -> Word16 -> ExifTag
toExifTag IFDGPS tg = toGPSTag tg
toExifTag _      tg = toStdTag tg

-- | Convert a Word16 number to standard Exif tag.
toStdTag :: Word16 -> ExifTag
toStdTag t = case t of
   0x0001 -> TagInteroperabilityIndex
   0x0002 -> TagInteroperabilityVersion
   0x0100 -> TagImageWidth
   0x0101 -> TagImageLength
   0x0102 -> TagBitsPerSample
   0x0103 -> TagCompression
   0x0106 -> TagPhotometricInterpretation
   0x010e -> TagImageDescription
   0x010f -> TagMake
   0x0110 -> TagModel
   0x0112 -> TagOrientation
   0x0115 -> TagSamplesPerPixel
   0x011a -> TagXResolution
   0x011b -> TagYResolution
   0x0128 -> TagResolutionUnit
   0x0131 -> TagSoftware
   0x0132 -> TagDateTime
   0x013b -> TagArtist
   0x013c -> TagHostComputer
   0x013e -> TagWhitePoint
   0x013f -> TagPrimaryChromaticities
   0x0201 -> TagJPEGInterchangeFormat
   0x0202 -> TagJPEGInterchangeFormatLength
   0x0211 -> TagYCbCrCoefficients
   0x0213 -> TagYCbCrPositioning
   0x0214 -> TagReferenceBlackWhite
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
   0x9296 -> TagSubjectDistance
   0x9207 -> TagMeteringMode
   0x9208 -> TagLightSource
   0x9209 -> TagFlash
   0x920a -> TagFocalLength
   0x927c -> TagMakerNote
   0x9286 -> TagUserComment
   0x9290 -> TagSubsecTime
   0x9291 -> TagSubSecTimeOriginal
   0x9292 -> TagSubSecTimeDigitized
   0x9c9b -> TagXPTitle
   0x9c9d -> TagXPAuthor
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
   0xa500 -> TagGamma
   0xc4a5 -> TagPrintImageMatching
   0xc6d2 -> TagPanasonicTitle1
   0xc6d3 -> TagPanasonicTitle2
   0xea1c -> TagPadding
   0xea1d -> TagOffsetSchemata
   0xff01 -> TagSubDirIFDMain
   0xff02 -> TagSubDirIFDExif
   0xff03 -> TagSubDirIFDInterop
   _ -> TagTagUnknown t

-- | Convert a Word16 number to an GPS tag
toGPSTag :: Word16 -> ExifTag
toGPSTag t = case t of
   0x0000 -> TagGPSVersionID
   0x0001 -> TagGPSLatitudeRef
   0x0002 -> TagGPSLatitude
   0x0003 -> TagGPSLongitudeRef
   0x0004 -> TagGPSLongitude
   0x0005 -> TagGPSAltitudeRef
   0x0006 -> TagGPSAltitude 
   0x0007 -> TagGPSTimeStamp
   0x0010 -> TagGPSImgDirectionRef
   0x0011 -> TagGPSImgDirection
   0x0012 -> TagGPSMapDatum
   0x0013 -> TagGPSDestLatitudeRef
   0x0014 -> TagGPSDestLatitude
   0x0015 -> TagGPSDestLongitudeRef
   0x0016 -> TagGPSDestLongitude
   0x001d -> TagGPSDateStamp
   0xff04 -> TagSubDirIFDGPS
   _ -> TagTagUnknown t
