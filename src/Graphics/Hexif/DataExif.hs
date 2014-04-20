-- -----------------------------------------------------------------------------
-- DataExif.hs
-- -----------------------------------------------------------------------------
--
-- Datastructures for the hexif library
-- 
-- -----------------------------------------------------------------------------

module Graphics.Hexif.DataExif where

import Data.Binary
import qualified Data.ByteString.Lazy as BL

data Exif = Exif [IFDDir] GetWords

-- Definiton of the resulting output
data ExifField = ExifField
    { exifTag :: ExifTag
    , value :: String 
    } deriving (Eq)

instance Show ExifField where
    show f = drop 3 (show $ exifTag f) ++ " -> " ++ (value f)


type IFDDir = [IFDEntry]

-- Definition of a logical IFD Entry
data IFDEntry = IFDRat ExifTag (Int, Int)
              | IFDNum ExifTag Int
              | IFDStr ExifTag String
              | IFDUdf ExifTag Int String
              | IFDSub DirTag IFDDir

-- Definition of a DirTag
data DirTag = IFDMain
         | IFDExif
         | IFDGPS
         | IFDInterop
     deriving (Eq, Show)

type GetWords = (Get Word16, Get Word32)

type IFDFileDir = [IFDFileEntry]
     

-- Definiton of physical IFD Entry in the file
data IFDFileEntry = IFDFileEntry
    { tag :: Word16                     -- 2 Bytes
    , format :: Word16       	        -- 2 Bytes
    , components :: Int   		        -- 4 Bytes
    , strValue :: BL.ByteString         -- 4 Bytes
    } deriving (Eq, Show)

-- Definition of all the supported Exif tags
data ExifTag = TagInteroperabilityIndex
             | TagInteroperabilityVersion
             | TagCompression
             | TagImageDescription
             | TagModel
             | TagMake
             | TagOrientation
             | TagTagUnknown Word16
             | TagXResolution
             | TagYResolution
             | TagResolutionUnit
             | TagSoftware
             | TagDateTime
             | TagArtist
             | TagJPEGInterchangeFormat
             | TagJPEGInterchangeFormatLength
             | TagYCbCrPositioning
             | TagRelatedImageWidth
             | TagRelatedImageLength
             | TagExposureTime
             | TagFNumber
             | TagExposureProgram
             | TagISOSpeedRatings
             | TagExifVersion
             | TagDateTimeOriginal
             | TagDateTimeDigitized
             | TagComponentsConfiguration
             | TagCompressedBitsPerPixel
             | TagBrightnessValue
             | TagExposureBiasValue
             | TagMaxApertureValue
             | TagMeteringMode
             | TagLightSource
             | TagFlash
             | TagFocalLength
             | TagMakerNote
             | TagUserComment
             | TagFlashPixVersion
             | TagColorSpace
             | TagPixelXDimension
             | TagPixelYDimension
             | TagFileSource
             | TagSceneType
             | TagCustomRendered
             | TagExposureMode
             | TagWhiteBalance
             | TagFocalLengthIn35mmFilm
             | TagSceneCaptureType
             | TagContrast
             | TagSaturation
             | TagSharpness
             | TagImageUniqueID
             | TagPrintImageMatching

             | TagGPSVersionID
             | TagGPSLatitudeRef
             | TagGPSLatitude
             | TagGPSLongitudeRef
             | TagGPSLogitude
             | TagGPSAltitudeRef
             | TagGPSAltitude 
             | TagGPSTimeStamp
             | TagGPSMapDatum
             | TagGPSDateStamp


			 | TagSubDir_IFDMain
             | TagSubDir_IFDExif
             | TagSubDir_IFDGPS
             | TagSubDir_IFDInterop
             
     deriving (Eq, Show)

