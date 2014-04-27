
-- | Datastructures for the hexif library

module Graphics.Hexif.DataExif where

import Data.Binary
import qualified Data.ByteString.Lazy as BL

-- | Datastructure with the interpreted Exif data
data Exif = Exif [IFDDataDir] GetWords

-- | Definiton of the resulting output
data ExifField = ExifField
    { exifTag :: ExifTag
    , value :: String 
    } deriving (Eq)

instance Show ExifField where
    show f = drop 3 (show $ exifTag f) ++ " -> " ++ (value f)

-- | A data directory is a list of data entries
type IFDDataDir = [IFDData]

-- | Definition of a logical IFD Entry together with the data
data IFDData = IFDRat ExifTag [(Int, Int)]
              | IFDNum ExifTag Int
              | IFDStr ExifTag String
              | IFDUdf ExifTag Int String
              | IFDSub DirTag IFDDataDir

-- | Definition of a DirTag
data DirTag = IFDMain
         | IFDExif
         | IFDGPS
         | IFDInterop
     deriving (Eq, Show)

type GetWords = (Get Word16, Get Word32)


type IFDFileDir = [IFDFileEntry]
     

-- | Representation of a physical IFD Entry in the file
data IFDFileEntry = IFDFileEntry
    { tag :: Word16                     -- 2 Bytes
    , format :: Word16       	        -- 2 Bytes
    , components :: Int   		        -- 4 Bytes
    , strValue :: BL.ByteString         -- 4 Bytes
    } deriving (Eq, Show)

-- | Definition of all the supported Exif tags
data ExifTag = TagInteroperabilityIndex
             | TagInteroperabilityVersion
             | TagImageWidth
             | TagImageLength
             | TagBitsPerSample
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
             | TagPrintImageMatching
             | TagPanasonicTitle1
             | TagPanasonicTitle2
             | TagPadding
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
			 | TagSubDir_IFDMain
             | TagSubDir_IFDExif
             | TagSubDir_IFDGPS
             | TagSubDir_IFDInterop
             
     deriving (Eq, Show)

