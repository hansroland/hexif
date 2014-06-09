
-- | Module with the datastructures for the hexif library.
-- This module is an internal module of Graphics.Hexif and should only be used
-- in the hexif project!

module Graphics.Hexif.DataExif where

import Data.Binary

-- | Datastructure with the interpreted Exif data.
data Exif = Exif DataBlock Encoding

-- | Definiton of the resulting output
data ExifField = ExifField ExifTag String   -- exifTag value
    deriving (Eq)

instance Show ExifField where
    show (ExifField exifTag value) = drop 3 (show exifTag) ++ " -> " ++ value

-- | A data directory is a list of data entries
type DataBlock = [DataEntry]

-- | Definition of a logical IFD Entry together with the data
data DataEntry = DataRat ExifTag Format [(Int, Int)]
             | DataNum ExifTag Format Int
             | DataStr ExifTag Format String
             | DataUdf ExifTag Format Int String
             | DataSub DirTag  DataBlock
     deriving (Eq, Show)

-- | The encoding of the binary data.
-- Motorola is big endian, Intel is low endian
data Encoding = Intel
    | Motorola

-- | Definition of a DirTag
data DirTag = IFDMain
         | IFDExif
         | IFDGPS
         | IFDInterop
     deriving (Eq, Show)


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
             | TagCFAPattern
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

