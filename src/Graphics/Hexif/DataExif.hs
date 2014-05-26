
-- | Module with the datastructures for the hexif library.
-- This module is an internal module of Graphics.Hexif and should only be used
-- in the hexif project!

module Graphics.Hexif.DataExif where

import Data.Binary
import qualified Data.ByteString.Lazy as BL

-- | Datastructure with the interpreted Exif data.
data Exif = Exif [IFDDir] GetWords

-- | Definiton of the resulting output
data ExifField = ExifField ExifTag String   -- exifTag value
    deriving (Eq)

instance Show ExifField where
    show (ExifField exifTag value) = drop 3 (show exifTag) ++ " -> " ++ value

-- | A data directory is a list of data entries
type IFDDir = [IFDData]

-- | Definition of a logical IFD Entry together with the data
data IFDData = IFDRat  ExifTag Format [(Int, Int)]
              | IFDNum ExifTag Format Int
              | IFDStr ExifTag Format String
              | IFDUdf ExifTag Format Int String
              | IFDSub DirTag  Format IFDDir

-- | Definition of a DirTag
data DirTag = IFDMain
         | IFDExif
         | IFDGPS
         | IFDInterop
     deriving (Eq, Show)

-- | Shortcut for the tuple to read 16 or 32 bits according to rhe Intel or 
--   Motorola format.
type GetWords = (Get Word16, Get Word32)

-- | A list of file entries builds a directory.
type IFDFileDir = [IFDFileEntry]
     

-- | Representation of a physical IFD Entry in the file
data IFDFileEntry = IFDFileEntry
    { tag :: Word16                     -- 2 Bytes
    , format :: Word16       	        -- 2 Bytes
    , components :: Int   		        -- 4 Bytes
    , strValue :: BL.ByteString         -- 4 Bytes
    } deriving (Eq, Show)

-- | Definitons of the Formats
data Format = Fmt00                      -- debug
            | Fmt01
            | Fmt02
            | Fmt03
            | Fmt04
            | Fmt05
            | Fmt06
            | Fmt07
            | Fmt08
            | Fmt09
            | Fmt10
      deriving (Eq, Show)

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
             | TagTagUnknown Word16
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
             | TagExifVersion
             | TagDateTimeOriginal
             | TagDateTimeDigitized
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
             | TagCustomRendered
             | TagExposureMode
             | TagWhiteBalance
             | TagDigitalZoomRatio
             | TagFocalLengthIn35mmFilm
             | TagSceneCaptureType
             | TagGainControl
             | TagContrast
             | TagSaturation
             | TagSharpness
             | TagSubjectDistanceRange
             | TagImageUniqueID
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
             -- Subdirectory tags
			 | TagSubDirIFDMain
             | TagSubDirIFDExif
             | TagSubDirIFDGPS
             | TagSubDirIFDInterop
     deriving (Eq, Show)

