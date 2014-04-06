-- -----------------------------------------------------------------------------
-- DataExif.hs
-- -----------------------------------------------------------------------------
--
-- Datastructures for the hexif library
-- 
-- -----------------------------------------------------------------------------

module DataExif where

import Data.Binary


-- Definiton of the resutlting output
data ExifField = ExifField
    { exifTag :: ExifTag
    , value :: String 
    } deriving (Eq)

-- Definition of a logical IFD Entry
data IFDEntry = IFDRat ExifTag Int Int
              | IFDNum ExifTag Int
              | IFDStr ExifTag String
              | IFDSub ExifTag IFDDir

-- Definition of a Tag
data DirTag = IFDMain
         | IFDExif
         | IFDGPS
         | IFDInterop


data IFDFileDir = IFDFileDir DirTag [IFDFileEntry]

data IFDDir = IFDDir DirTag [IFDEntry]

-- Definiton of physical IFD Entry in the file
data IFDFileEntry = IFDFileEntry
    { tag :: Word16             -- 2 Bytes
    , format :: Word16       	-- 2 Bytes
    , components :: Int   		-- 4 Bytes
    , offsetOrValue :: Int		-- 4 Bytes
    , strValue :: String
    } deriving (Eq, Show)


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
             | TagISOSpeedRatings
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
             | TagFlashPixVersion
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

