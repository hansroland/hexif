-- ---------------------------------------------------------------------------------------
-- Exif.hs - Read and Parse the Exif File of a JPEG File
-- ---------------------------------------------------------------------------------------
--
-- Read (and later write) the exif file of a JPEG image with 
--     native Haskell code
--
-- See: 
--    http://www.media.mit.edu/pia/Research/deepview/exif.html
--    http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif.html   
--
--    
--
-- ----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Exif where

import Data.Binary
import Data.Binary.Get   {-( Get
                      , getWord8
                      , getWord16be
                      , getByteString
                      , skip
                      , bytesRead
                      )   -}
import Data.List
import Data.Char
import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

-- ------------------------------------------------------------------------------
-- Data Structures
-- ------------------------------------------------------------------------------
--
-- See: http://hackage.haskell.org/package/binary-0.7.1.0/docs/Data-Binary-Get.html

-- Exif Data Structure
data Exif = Exif
    { nOffset :: Int
    } deriving (Eq, Show)


data IFDEntry = IFDEntry
    { tag :: Word16             -- 2 Bytes
    , format :: Word16       	-- 2 Bytes
    , components :: Int   		-- 4 Bytes
    , offsetOrValue :: Int		-- 4 Bytes
    } deriving (Eq, Show)

data ExifField = ExifField
    { exifTag :: ExifTag
    , value :: BL.ByteString 
    } deriving (Eq)

instance Show ExifField where
    show f = drop 3 (show $ exifTag f) ++ " -> " ++ (unpackStr $ value f)


type GetWords = (Get Word16, Get Word32)

-- read in the exif data structure
-- getExif :: Get Exif
getExif :: BL.ByteString -> Get [ExifField]
getExif input = do
    -- Tiff Header 
    tiffAlign <- getWord16be
    let getWords = if fromIntegral tiffAlign == 0x4949
        then (getWord16le, getWord32le)
        else (getWord16be, getWord32be) 
    const2A <- getByteString 2
    -- Image File Directory Blocks
    nOffset <- snd getWords
    blocks <- getIFDBlocks nOffset getWords
    let entries = concat blocks
    let (usefuls, pointers) = partition (\e -> tag e /= 0x8769) entries
    let tags = map (toExifField input getWords) usefuls
    additional <- if (length pointers > 0)
        then getIFDBlocks (fromIntegral $ offsetOrValue $ head pointers)  getWords
        else return []
    return $ map (toExifField input getWords) (usefuls ++ concat additional)
   


-- read IFD blocks
getIFDBlocks :: Word32 -> GetWords -> Get [[IFDEntry]]
getIFDBlocks nOffset getWords = do

    if nOffset == 0
        then return []
        else do 
           pos <- bytesRead
           skip $ (fromIntegral nOffset) - (fromIntegral pos)
           nEntries <- fst getWords
           block <- getIFDEntries (fromIntegral nEntries) getWords
           next <- snd getWords
           blocks <- getIFDBlocks next getWords
           return $ block : blocks

-- read a single IFD block. It contains n IFD entries. 
getIFDEntries :: Int -> GetWords -> Get [IFDEntry]
getIFDEntries n getWords =
    if n == 0 
        then return []
        else do
           entry <- getIFDEntry getWords
           entries <- getIFDEntries (n - 1) getWords
           return $ entry : entries


-- read a single IFD entry
getIFDEntry ::  GetWords -> Get IFDEntry
getIFDEntry getWords = do
    let getWord16 = fst getWords
    let getWord32 = snd getWords
    tagNr <- getWord16
    format <- getWord16
    comps <- getWord32
    offset <- getWord32
    return $ IFDEntry tagNr format (fromIntegral comps) (fromIntegral offset)
    
    

-- convert a IFD entry to an ExifField
toExifField :: BL.ByteString -> GetWords -> IFDEntry -> ExifField
toExifField bsExif words (IFDEntry tag format len offsetOrValue) = 
    case format of
        0x0002 -> ExifField exifTag $ getStringValue len offsetOrValue bsExif
        0x0003 -> ExifField exifTag $ decodeTagValue exifTag offsetOrValue
        0x0004 -> ExifField exifTag $ decodeTagValue exifTag offsetOrValue
        0x0005 -> ExifField exifTag $ getRationalValue offsetOrValue bsExif words
        0x0007 -> ExifField exifTag $ packStr $ (show len) ++ " bytes undefined data"
        0x000A -> ExifField exifTag $ getRationalValue offsetOrValue bsExif words   -- signed rationale !!
        _      -> error $ "Format " ++ show format ++ " not yet implemented" 
    where exifTag = toExifTag tag
    

    
-- subfunctions of toExifField   
getStringValue :: Int -> Int -> BL.ByteString -> BL.ByteString
getStringValue len offset bsExif = runGet (getString len offset) bsExif
     
getString :: Int -> Int -> Get BL.ByteString
getString len offset = do

    skip offset
    getLazyByteString $ fromIntegral (len - 1) 

 

getRationalValue :: Int -> BL.ByteString -> GetWords -> BL.ByteString
getRationalValue offset bsExif words = runGet (getRationale words offset) bsExif

   

getRationale :: GetWords -> Int -> Get BL.ByteString
getRationale words offset = do
   skip offset
   let getWord32 = snd words
   numerator <- getWord32
   denumerator <- getWord32
   return $ packStr $ (show numerator) ++ "/" ++ (show denumerator)
   

-- Run it
example0 :: IO()
example0 = do
   inp <- BL.readFile "JG1111.exif"
   let input = BL.drop 6 inp
   let fields = runGet (getExif input) input
   mapM_ print fields



example1 :: IO()
example1 = do
   input <- BL.readFile "JG1111.exif"
   print $ getStringValue 5 (178 + 6) input


-- translate numerical tag values to the corresponding ByteString value
decodeTagValue :: ExifTag -> Int -> BL.ByteString
decodeTagValue TagCompression n    = decodeCompression n
decodeTagValue TagResolutionUnit n = decodeResolutionUnit n
decodeTagValue TagOrientation n    = decodeOrientation n
decodeTagValue TagYCbCrPositioning n = decodeYCbCrPositioning n
decodeTagValue _ n = (packStr . show) n

-- interpretation of Resolution Unit
decodeResolutionUnit :: Int -> BL.ByteString
decodeResolutionUnit 1 = "No absolute unit"
decodeResolutionUnit 2 = "Inch"
decodeResolutionUnit 3 = "Centimeter"
decodeResulutionUnit n = undef n

-- interpretation of Orientation 
decodeOrientation :: Int -> BL.ByteString
decodeOrientation 1 = "Top-left"
decodeOrientation 2 = "Top-right" 
decodeOrientation 3 = "Bottom-right"
decodeOrientation 4 = "Bottom-left"
decodeOrientation 5 = "Left-top"
decodeOrientation 6 = "Right-top"
decodeOrientation 7 = "Right-bottom"
decodeOrientation 8 = "Left-bottom"
decodeOrientation n = undef n

decodeYCbCrPositioning :: Int -> BL.ByteString
decodeYCbCrPositioning 1 = "Centered"
decodeYCbCrPositioning 2 = "Co-sited"
decodeYCbCrPositioning n = undef n

decodeCompression :: Int -> BL.ByteString
decodeCompression 1 = "No compression"
decodeCompression 2 = "CCITT modified Huffman RLE"
decodeCompression 3 = "CCITT Group 3 fax"
decodeCompression 4 = "CCITT Group 4 fax"
decodeCompression 5 = "LZW"
decodeCompression 6 = "JPEG (old style)"
decodeCompression 7 = "JPEG (new style)"
decodeCompression n = undef n


-- little support function: normal pack is refused by GHC
-- Hoogle says: pack :: String -> BL.ByteString
-- GHCi says:   pack :: pack :: [Word8] -> BL.ByteString
-- Why is Haskell string conversion so difficult?
packStr :: String -> BL.ByteString
packStr = BL.pack . map (fromIntegral . ord)

unpackStr :: BL.ByteString -> String
unpackStr = map (chr . fromIntegral)  . BL.unpack

undef :: Int -> BL.ByteString
undef n = BL.append "undefined "   ((packStr . show) n)

-- Definition of all the supported Exif tags
data ExifTag = TagCompression
             | TagImageDescription
             | TagModel
             | TagMake
             | TagOrientation
             | TagTagUnknown Word16
             | TagXResolution
             | TagYResolution
             | TagResolutionUnit
             | TagDateTime
             | TagJPEGInterchangeFormat
             | TagJPEGInterchangeFormatLength
             | TagYCbCrPositioning
             | TagExposureTime
             | TagFNumber
             | TagExposureProgram
 
             | TagExifVersion

             | TagDateTimeOriginal
             | TagDateTimeDigitized
             | TagComponentsConfiguration
             | TagCompressedBitsPerPixel
             | TagExposureBiasValue
             | TagMaxApertureValue
             | TagMeteringMode
             | TagLightSource
             | TagFlash
             | TagFocalLength
             | TagMakerNote
             | TagFlashpixVersion
             | TagColorSpace
             | TagPixelXDimension
             | TagPixelYDimension
             | TagFileSource
             | TagSceneType
             | TagCustomRendered
             | TagExposureMode
             | TagWhiteBalance
             | TagSceneCaptureType
             | TagPrintImageMatching
     deriving (Eq, Show)


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
   | t == 0xa000 = TagFlashpixVersion
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



{-
-- use markerIntValue to compare with a marker eg 0xFFD8
markerIntValue :: JpegSegment -> Int
markerIntValue = fromIntegral . runGet getVal . segMarker
   where getVal = do
         getWord16be
-}


{-

data IfdType = TypeByte
             | TypeAscii
             | TypeShort
             | TypeLong
             | TypeRational
             | TypeSByte
             | TypeUndefined
             | TypeSignedShort
             | TypeSignedLong
             | TypeSignedRational
             | TypeFloat
             | TypeDouble


instance BinaryParam Endianness IfdType where
    getP endianness = getP endianness >>= conv
      where
        conv :: Word16 -> Get IfdType
        conv 1  = return TypeByte
        conv 2  = return TypeAscii
        conv 3  = return TypeShort
        conv 4  = return TypeLong
        conv 5  = return TypeRational
        conv 6  = return TypeSByte
        conv 7  = return TypeUndefined
        conv 8  = return TypeSignedShort
        conv 9  = return TypeSignedLong
        conv 10 = return TypeSignedRational
        conv 11 = return TypeFloat
        conv 12 = return TypeDouble
        conv _  = fail "Invalid TIF directory type"



/*! EXIF tags */
typedef enum {
	EXIF_TAG_INTEROPERABILITY_INDEX		= 0x0001,
	EXIF_TAG_INTEROPERABILITY_VERSION	= 0x0002,
	EXIF_TAG_NEW_SUBFILE_TYPE		= 0x00fe,
	EXIF_TAG_IMAGE_WIDTH 			= 0x0100,
	EXIF_TAG_IMAGE_LENGTH 			= 0x0101,
	EXIF_TAG_BITS_PER_SAMPLE 		= 0x0102,
	-- EXIF_TAG_COMPRESSION 			= 0x0103,
	EXIF_TAG_PHOTOMETRIC_INTERPRETATION 	= 0x0106,
	EXIF_TAG_FILL_ORDER 			= 0x010a,
	EXIF_TAG_DOCUMENT_NAME 			= 0x010d,
	-- EXIF_TAG_IMAGE_DESCRIPTION 		= 0x010e,
	-- EXIF_TAG_MAKE 				= 0x010f,
	-- EXIF_TAG_MODEL 				= 0x0110,
	EXIF_TAG_STRIP_OFFSETS 			= 0x0111,
	-- EXIF_TAG_ORIENTATION 			= 0x0112,
	EXIF_TAG_SAMPLES_PER_PIXEL 		= 0x0115,
	EXIF_TAG_ROWS_PER_STRIP 		= 0x0116,
	EXIF_TAG_STRIP_BYTE_COUNTS		= 0x0117,
	-- EXIF_TAG_X_RESOLUTION 			= 0x011a,
	-- EXIF_TAG_Y_RESOLUTION 			= 0x011b,
	EXIF_TAG_PLANAR_CONFIGURATION 		= 0x011c,
	-- EXIF_TAG_RESOLUTION_UNIT 		= 0x0128,
	EXIF_TAG_TRANSFER_FUNCTION 		= 0x012d,
	EXIF_TAG_SOFTWARE 			= 0x0131,
	-- EXIF_TAG_DATE_TIME			= 0x0132,
	EXIF_TAG_ARTIST				= 0x013b,
	EXIF_TAG_WHITE_POINT			= 0x013e,
	EXIF_TAG_PRIMARY_CHROMATICITIES		= 0x013f,
	EXIF_TAG_SUB_IFDS			= 0x014a,
	EXIF_TAG_TRANSFER_RANGE			= 0x0156,
	EXIF_TAG_JPEG_PROC			= 0x0200,
	-- EXIF_TAG_JPEG_INTERCHANGE_FORMAT	= 0x0201,
	-- EXIF_TAG_JPEG_INTERCHANGE_FORMAT_LENGTH	= 0x0202,
	EXIF_TAG_YCBCR_COEFFICIENTS		= 0x0211,
	EXIF_TAG_YCBCR_SUB_SAMPLING		= 0x0212,
	-- EXIF_TAG_YCBCR_POSITIONING		= 0x0213,
	EXIF_TAG_REFERENCE_BLACK_WHITE		= 0x0214,
	EXIF_TAG_XML_PACKET			= 0x02bc,
	EXIF_TAG_RELATED_IMAGE_FILE_FORMAT	= 0x1000,
	EXIF_TAG_RELATED_IMAGE_WIDTH		= 0x1001,
	EXIF_TAG_RELATED_IMAGE_LENGTH		= 0x1002,
	EXIF_TAG_CFA_REPEAT_PATTERN_DIM		= 0x828d,
	EXIF_TAG_CFA_PATTERN			= 0x828e,
	EXIF_TAG_BATTERY_LEVEL			= 0x828f,
	EXIF_TAG_COPYRIGHT			= 0x8298,
	-- EXIF_TAG_EXPOSURE_TIME			= 0x829a,
	-- EXIF_TAG_FNUMBER			= 0x829d,
	EXIF_TAG_IPTC_NAA			= 0x83bb,
	EXIF_TAG_IMAGE_RESOURCES		= 0x8649,
	-- EXIF_TAG_EXIF_IFD_POINTER		= 0x8769,
	EXIF_TAG_INTER_COLOR_PROFILE		= 0x8773,
	-- EXIF_TAG_EXPOSURE_PROGRAM		= 0x8822,
	EXIF_TAG_SPECTRAL_SENSITIVITY		= 0x8824,
	EXIF_TAG_GPS_INFO_IFD_POINTER		= 0x8825,
	EXIF_TAG_ISO_SPEED_RATINGS		= 0x8827,
	EXIF_TAG_OECF				= 0x8828,
	EXIF_TAG_TIME_ZONE_OFFSET		= 0x882a,
	-- EXIF_TAG_EXIF_VERSION			= 0x9000,
	-- EXIF_TAG_DATE_TIME_ORIGINAL		= 0x9003,
	-- EXIF_TAG_DATE_TIME_DIGITIZED		= 0x9004,
	-- EXIF_TAG_COMPONENTS_CONFIGURATION	= 0x9101,
	-- EXIF_TAG_COMPRESSED_BITS_PER_PIXEL	= 0x9102,
	EXIF_TAG_SHUTTER_SPEED_VALUE		= 0x9201,
	EXIF_TAG_APERTURE_VALUE			= 0x9202,
	EXIF_TAG_BRIGHTNESS_VALUE		= 0x9203,
	-- EXIF_TAG_EXPOSURE_BIAS_VALUE		= 0x9204,
	-- EXIF_TAG_MAX_APERTURE_VALUE		= 0x9205,
	EXIF_TAG_SUBJECT_DISTANCE		= 0x9206,
	-- EXIF_TAG_METERING_MODE			= 0x9207,
	-- EXIF_TAG_LIGHT_SOURCE			= 0x9208,
	-- EXIF_TAG_FLASH				= 0x9209,
	-- EXIF_TAG_FOCAL_LENGTH			= 0x920a,
	EXIF_TAG_SUBJECT_AREA			= 0x9214,
	EXIF_TAG_TIFF_EP_STANDARD_ID		= 0x9216,
	-- EXIF_TAG_MAKER_NOTE			= 0x927c,
	EXIF_TAG_USER_COMMENT			= 0x9286,
	EXIF_TAG_SUB_SEC_TIME			= 0x9290,
	EXIF_TAG_SUB_SEC_TIME_ORIGINAL		= 0x9291,
	EXIF_TAG_SUB_SEC_TIME_DIGITIZED		= 0x9292,
	EXIF_TAG_XP_TITLE			= 0x9c9b,
	EXIF_TAG_XP_COMMENT			= 0x9c9c,
	EXIF_TAG_XP_AUTHOR			= 0x9c9d,
	EXIF_TAG_XP_KEYWORDS			= 0x9c9e,
	EXIF_TAG_XP_SUBJECT			= 0x9c9f,
	-- EXIF_TAG_FLASH_PIX_VERSION		= 0xa000,
	-- EXIF_TAG_COLOR_SPACE			= 0xa001,
	-- EXIF_TAG_PIXEL_X_DIMENSION		= 0xa002,
	-- EXIF_TAG_PIXEL_Y_DIMENSION		= 0xa003,
	EXIF_TAG_RELATED_SOUND_FILE		= 0xa004,
	EXIF_TAG_INTEROPERABILITY_IFD_POINTER	= 0xa005,
	EXIF_TAG_FLASH_ENERGY			= 0xa20b,
	EXIF_TAG_SPATIAL_FREQUENCY_RESPONSE	= 0xa20c,
	EXIF_TAG_FOCAL_PLANE_X_RESOLUTION	= 0xa20e,
	EXIF_TAG_FOCAL_PLANE_Y_RESOLUTION	= 0xa20f,
	EXIF_TAG_FOCAL_PLANE_RESOLUTION_UNIT	= 0xa210,
	EXIF_TAG_SUBJECT_LOCATION		= 0xa214,
	EXIF_TAG_EXPOSURE_INDEX			= 0xa215,
	EXIF_TAG_SENSING_METHOD			= 0xa217,
	-- EXIF_TAG_FILE_SOURCE			= 0xa300,
	-- EXIF_TAG_SCENE_TYPE			= 0xa301,
	EXIF_TAG_NEW_CFA_PATTERN		= 0xa302,
	-- EXIF_TAG_CUSTOM_RENDERED		= 0xa401,
	-- EXIF_TAG_EXPOSURE_MODE			= 0xa402,
	-- EXIF_TAG_WHITE_BALANCE			= 0xa403,
	EXIF_TAG_DIGITAL_ZOOM_RATIO		= 0xa404,
	EXIF_TAG_FOCAL_LENGTH_IN_35MM_FILM	= 0xa405,
	-- EXIF_TAG_SCENE_CAPTURE_TYPE		= 0xa406,
	EXIF_TAG_GAIN_CONTROL			= 0xa407,
	EXIF_TAG_CONTRAST			= 0xa408,
	EXIF_TAG_SATURATION			= 0xa409,
	EXIF_TAG_SHARPNESS			= 0xa40a,
	EXIF_TAG_DEVICE_SETTING_DESCRIPTION	= 0xa40b,
	EXIF_TAG_SUBJECT_DISTANCE_RANGE		= 0xa40c,
	EXIF_TAG_IMAGE_UNIQUE_ID		= 0xa420,
	EXIF_TAG_GAMMA				= 0xa500,
	-- EXIF_TAG_PRINT_IMAGE_MATCHING		= 0xc4a5,
	EXIF_TAG_PADDING			= 0xea1c
} ExifTag;



-}










