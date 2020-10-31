module Graphics.Hexif.Types

  where

import Graphics.Hexif.Utils
import Data.Binary
import qualified Data.Map as Map

-- A datastructure containing all the Exif values
data Exif a = Exif
    { encoding  :: Encoding
    , ifdMap    :: Map.Map Word16 a
    }
  deriving (Show)

-- IFD : Image file directory
-- TODO: Use a newtype !!
type RawIfd = Map.Map Word16 RawEntry

data RawEntry = RawEntry
    { rawEntryTag :: Word16
    , rawDataType :: Word16
    , rawCount    :: Word32
    , rawValue16  :: Word16
    , rawValue32  :: Word32
    , rawString   :: String
    }

instance Show RawEntry where
  show entry = "\n" ++
    showHex (rawEntryTag entry) ++ " " ++
    show (rawDataType entry) ++ " " ++
    show (rawCount entry) ++ "  " ++
    show (rawValue32 entry) ++ " " ++
    showHex (rawValue32 entry)

-- IFD : Image file directory
-- TODO: Create newtype
type Ifd = Map.Map ExifTag IfdEntry

-- Subdirectory Entries
data IfdEntry = IfdEntry
    { entryTag        :: ExifTag
    , entryValue      :: ExifValue
    }
instance Show IfdEntry where
  show entry = "\n" ++ show (entryTag entry) ++ "  " ++ show (entryValue entry)

data PrettyEntry  = PrettyEntry
  { prettyTag   :: String
  , prettyValue :: String
  }

instance Show PrettyEntry where
  show e = prettyTag e ++ " -> " ++  prettyValue e

--
data ExifValue =
    ValueInt Int
    | ValueStr String
    | ValueUdf Int String
    | ValueRat [(Int, Int)]
  deriving (Show)

exifValueString :: ExifValue -> Maybe String
exifValueString (ValueStr str) = Just str
exifValueString _              = Nothing

-- TODO: Add the 0x0000 tag!!
-- | IFD Tags
tagExifIfd :: Word16
tagExifIfd  = 0x8769

tagGpsIfd :: Word16
tagGpsIfd   = 0x8825

tagInterIfd :: Word16
tagInterIfd = 0xa005

tagsMap :: Map.Map Word16 (Word16 -> ExifTag)
tagsMap = Map.fromList [(0x0000, toStdTag)
                       , (tagExifIfd, toStdTag)
                       , (tagGpsIfd,  toGpsTag)
                       , (tagInterIfd, toIopTag)]

ifdTags :: [Word16]
ifdTags = [tagExifIfd, tagGpsIfd, tagInterIfd]

-- | Definition of all the supported Exif tags
data ExifTag = TagInteroperabilityIndex
             | TagInteroperabilityVersion
             | TagImageWidth
             | TagImageLength
             | TagBitsPerSample
             | TagCompression
             | TagPhotometricInterpretation
             | TagImageDescription
             | TagModel
             | TagMake
             | TagOrientation
             | TagSamplesPerPixel
             | TagTagUnknown String
             | TagXResolution
             | TagYResolution
             | TagResolutionUnit
             | TagSoftware
             | TagDateTime
             | TagArtist
             | TagHostComputer
             | TagWhitePoint
             | TagPrimaryChromaticities
             | TagJPEGInterchangeFormat
             | TagJPEGInterchangeFormatLength
             | TagYCbCrCoefficients
             | TagYCbCrPositioning
             | TagReferenceBlackWhite
             | TagRelatedImageWidth
             | TagRelatedImageLength
             | TagCopyright
             | TagExposureTime
             | TagFNumber
             | TagExposureProgram
             | TagISOSpeedRatings
             | TagSensitivityType
             | TagExifVersion
             | TagDateTimeOriginal
             | TagDateTimeDigitized
             | TagOffsetTime
             | TagOffsetTimeOriginal
             | TagOffsetTimeDigitized
             | TagComponentsConfiguration
             | TagCompressedBitsPerPixel
             | TagShutterSpeedValue
             | TagApertureValue
             | TagBrightnessValue
             | TagExposureBiasValue
             | TagMaxApertureValue
             | TagSubjectDistance
             | TagMeteringMode
             | TagLightSource
             | TagFlash
             | TagFocalLength
             | TagMakerNote
             | TagUserComment
             | TagSubsecTime
             | TagSubSecTimeOriginal
             | TagSubSecTimeDigitized
             | TagXPTitle
             | TagXPAuthor
             | TagFlashPixVersion
             | TagColorSpace
             | TagPixelXDimension
             | TagPixelYDimension
             | TagFocalPlaneXResolution
             | TagFocalPlaneYResolution
             | TagFocalPlaneResolutionUnit
             | TagSensingMethod
             | TagFileSource
             | TagSceneType
             | TagCFAPattern
             | TagCustomRendered
             | TagExposureMode
             | TagWhiteBalance
             | TagDigitalZoomRatio
             | TagFocalLengthIn35mmFilm
             | TagSubjectArea
             | TagSceneCaptureType
             | TagGainControl
             | TagContrast
             | TagSaturation
             | TagSharpness
             | TagSubjectDistanceRange
             | TagImageUniqueID
             | TagLensInfo
             | TagLensMake
             | TagLensModel
             | TagLensSerialModel
             | TagGamma
             | TagPrintImageMatching
             | TagPanasonicTitle1
             | TagPanasonicTitle2
             | TagPadding
             | TagOffsetSchemata
             -- GPS Tags
             | TagGPSVersionID
             | TagGPSLatitudeRef
             | TagGPSLatitude
             | TagGPSLongitudeRef
             | TagGPSLongitude
             | TagGPSAltitudeRef
             | TagGPSAltitude
             | TagGPSTimeStamp
             | TagGPSImgDirectionRef
             | TagGPSImgDirection
             | TagGPSMapDatum
             | TagGPSDestLatitudeRef
             | TagGPSDestLatitude
             | TagGPSDestLongitudeRef
             | TagGPSDestLongitude
             | TagGPSDateStamp
             | TagGPSSatelites
             | TagGPSStatus
             | TagGPS0a
             | TagGPS0b
             | TagGPSSpeedRef
             | TagGPSSpeed
             | TagGPSTrackRef
             | TagGPS0f
             | TagGPSDestBearingRef
             | TagGPS18
             | TagGPSAreaInformation
             | TagGPS1F
     deriving (Eq, Ord, Show)


-- | Convert a Word16 number to standard Exif tag.
toStdTag :: Word16 -> ExifTag
toStdTag t = case t of
   0x0100 -> TagImageWidth
   0x0101 -> TagImageLength
   0x0102 -> TagBitsPerSample
   0x0103 -> TagCompression
   0x0106 -> TagPhotometricInterpretation
   0x011a -> TagXResolution
   0x011b -> TagYResolution
   0x010e -> TagImageDescription
   0x010f -> TagMake
   0x0110 -> TagModel
   0x0112 -> TagOrientation
   0x0115 -> TagSamplesPerPixel
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
   0x8830 -> TagSensitivityType
   0x9000 -> TagExifVersion
   0x9003 -> TagDateTimeOriginal
   0x9004 -> TagDateTimeDigitized
   0x9010 -> TagOffsetTime
   0x9011 -> TagOffsetTimeOriginal
   0x9012 -> TagOffsetTimeDigitized
   0x9101 -> TagComponentsConfiguration
   0x9102 -> TagCompressedBitsPerPixel
   0x9201 -> TagShutterSpeedValue
   0x9202 -> TagApertureValue
   0x9203 -> TagBrightnessValue
   0x9204 -> TagExposureBiasValue
   0x9205 -> TagMaxApertureValue
   0x9206 -> TagSubjectDistance
   0x9207 -> TagMeteringMode
   0x9208 -> TagLightSource
   0x9209 -> TagFlash
   0x920a -> TagFocalLength
   0x9214 -> TagSubjectArea
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
   0xa302 -> TagCFAPattern
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
   0xa432 -> TagLensInfo
   0xa433 -> TagLensMake
   0xa434 -> TagLensModel
   0xa435 -> TagLensSerialModel
   0xa500 -> TagGamma
   0xc4a5 -> TagPrintImageMatching
   0xc6d2 -> TagPanasonicTitle1
   0xc6d3 -> TagPanasonicTitle2
   0xea1c -> TagPadding
   0xea1d -> TagOffsetSchemata
   _ -> TagTagUnknown (showHex t)

toIopTag :: Word16 -> ExifTag
toIopTag t = case t of
    0x0001 -> TagInteroperabilityIndex
    0x0002 -> TagInteroperabilityVersion
    0x1001 -> TagRelatedImageWidth
    0x1002 -> TagRelatedImageLength
    _ -> TagTagUnknown (showHex t)

-- | Convert a Word16 number to an GPS tag
toGpsTag :: Word16 -> ExifTag
toGpsTag t = case t of
   0x0000 -> TagGPSVersionID
   0x0001 -> TagGPSLatitudeRef
   0x0002 -> TagGPSLatitude
   0x0003 -> TagGPSLongitudeRef
   0x0004 -> TagGPSLongitude
   0x0005 -> TagGPSAltitudeRef
   0x0006 -> TagGPSAltitude
   0x0007 -> TagGPSTimeStamp
   0x0008 -> TagGPSSatelites
   0x0009 -> TagGPSStatus
   0x000a -> TagGPS0a
   0x000b -> TagGPS0b
   0x000c -> TagGPSSpeedRef
   0x000d -> TagGPSSpeed
   0x000e -> TagGPSTrackRef
   0x000f -> TagGPS0f
   0x0010 -> TagGPSImgDirectionRef
   0x0011 -> TagGPSImgDirection
   0x0012 -> TagGPSMapDatum
   0x0013 -> TagGPSDestLatitudeRef
   0x0014 -> TagGPSDestLatitude
   0x0015 -> TagGPSDestLongitudeRef
   0x0016 -> TagGPSDestLongitude
   0x0017 -> TagGPSDestBearingRef
   0x0018 -> TagGPS18
   0x001c -> TagGPSAreaInformation
   0x001d -> TagGPSDateStamp
   0x001f -> TagGPS1F
   _ -> TagTagUnknown  (showHex t)

data ExifDataType
    = TypeByte          --  1 - An 8-bit unsigned integer.,
    | TypeAscii         --  2 - An 8-bit byte containing one 7-bit ASCII code.
                        --      The final byte is terminated with NULL.,
    | TypeShort         --  3 - A 16-bit (2-byte) unsigned integer,
    | TypeLong          --  4 - A 32-bit (4-byte) unsigned integer,
    | TypeRational      --  5 - Two LONGs. Numerator and denominator.,
    | TypeUndefined     --  7 - An 8-bit byte that can take any value depending on the field definition,
    | TypeSlong         --  9 - A 32-bit (4-byte) signed integer (2's complement notation),
    | TypeSrational     -- 10 - Two SLONGs. Numerator and denominator.
  deriving (Eq, Show)

