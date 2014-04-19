-- -----------------------------------------------------------------------------
-- PrettyPrint.hs
-- -----------------------------------------------------------------------------
--
-- Print out the contents of IFDEntries
--
--
-- -----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Graphics.Hexif.PrettyPrint (prettyPrint) where

import Graphics.Hexif.DataExif
import Graphics.Hexif.Utils
import Text.Printf (printf)

import Data.Char (chr, ord)
-- import Data.String.Utils(join)

prettyPrint :: IFDDir -> [ExifField]
prettyPrint entries = map ppIFDEntry (flatten entries)

-- Note: suDirs will always be printed at the end
flatten :: IFDDir -> [IFDEntry]
flatten [] = []
flatten ((IFDSub tag ifdDir) : ds) = flatten ds ++ flatten ifdDir
flatten (d : ds) = d : flatten ds

ppIFDEntry :: IFDEntry -> ExifField 
ppIFDEntry (IFDRat tag numdenom) = ExifField tag (ppRationalValue tag numdenom)
ppIFDEntry (IFDNum tag nVal) = ExifField tag (ppNumValue tag nVal)
ppIFDEntry (IFDStr tag strVal) = ExifField tag strVal
ppIFDEntry (IFDUdf tag len strVal) = ExifField tag (ppUndefinedValue tag len strVal)
ppIFDEntry (IFDSub tag ifdDir) = error $ "Directory encountered " ++ show tag

-- ----------------------------------------------------------------------------
-- Pretty Printers for RationalValues
-- ----------------------------------------------------------------------------
ppRationalValue :: ExifTag -> (Int,Int) -> String
ppRationalValue TagExposureTime r = fmtRatWithSlash r ++ " sec."
ppRationalValue TagFNumber r = "f/" ++ fmtRatFloat r
ppRationalValue TagCompressedBitsPerPixel r = ' ' : fmtRat r
ppRationalValue TagExposureBiasValue r = ppExposureBiasValue r
ppRationalValue TagFocalLength r = ppFocalLength r
ppRationalValue _  rat = fmtRat rat

--convert a rational to a float
rat2Float :: (Int,Int) -> Float
rat2Float (n,d) = (fromIntegral n::Float) / (fromIntegral d:: Float)


-- format a rational number with a slash
fmtRatWithSlash :: (Int, Int) -> String
fmtRatWithSlash (num,denum) =
    show (div num ggt) ++ "/" ++ show (div denum ggt)
    where ggt = gcd num denum

fmtRat :: (Int, Int) -> String
fmtRat r@(num, denum) = 
     if mod num denum == 0 then fmtRatInt r else fmtRatFloat r


fmtRatInt :: (Int, Int) -> String
fmtRatInt (num, denum) = show $ div num denum
 

fmtRatFloat :: (Int, Int) -> String
fmtRatFloat = show . rat2Float

-- Pretty print the value of the tag ExposureBiasValue
ppExposureBiasValue :: (Int, Int) -> String
ppExposureBiasValue r = printf "%.2f EV" (rat2Float r)

-- Pretty print the value of the tag FocalLength
ppFocalLength :: (Int, Int) -> String
ppFocalLength r = printf "%.1f mm" (rat2Float r)
    

-- ----------------------------------------------------------------------------
-- Pretty Printers for Undefined Values
-- ----------------------------------------------------------------------------
ppUndefinedValue :: ExifTag -> Int -> String -> String
ppUndefinedValue TagExifVersion len value = ppExifVersion value
ppUndefinedValue TagFlashPixVersion len value = ppFlashPixVersion value
ppUndefinedValue TagComponentsConfiguration len value = ppComponentsConfiguration value
ppUndefinedValue TagFileSource len value = ppFileSource value
ppUndefinedValue TagSceneType len value = ppScreenType value
ppUndefinedValue TagInteroperabilityVersion len value = value
ppUndefinedValue _ len _ = show len ++ " bytes undefined data"

ppExifVersion :: String -> String
ppExifVersion value = "Exif Version " ++ show num
  where num :: Float = read value / 100.0

ppFlashPixVersion :: String -> String
ppFlashPixVersion value = printf "FlashPix Version %.1f" num
  where num :: Float = read value / 100.0

ppComponentsConfiguration :: String -> String
ppComponentsConfiguration conf = join " " $ map ppComps conf
   where
       ppComps (ord -> 0)  = "-"
       ppComps (ord -> 1) = "Y"
       ppComps (ord -> 2) = "Cb"
       ppComps (ord -> 3) = "Cr"
       ppComps (ord -> 4) = "R"
       ppComps (ord -> 5) = "G"
       ppComps (ord -> 6) = "B"


ppFileSource :: String -> String
ppFileSource value = 
      if head value == chr 3 
      then "DSC" 
      else "(unknown)"

ppScreenType :: String -> String
ppScreenType value = 
      if head value == chr 1 
      then "Directly photographed" 
      else "(unknown)"
   
-- -----------------------------------------------------------------------------
-- PrettyPrint functions for Integer values
-- -----------------------------------------------------------------------------
ppNumValue :: ExifTag -> Int -> String
ppNumValue TagCompression n    = ppCompression n
ppNumValue TagResolutionUnit n = ppResolutionUnit n
ppNumValue TagOrientation n    = ppOrientation n
ppNumValue TagYCbCrPositioning n = ppYCbCrPositioning n
ppNumValue TagExposureProgram n = ppTagExposureProgram n
ppNumValue TagMeteringMode n = ppTagMeteringMode n
ppNumValue TagLightSource n = ppTagLightSource n
ppNumValue TagFlash n = ppTagFlash n
ppNumValue TagColorSpace n = ppTagColorSpace n
ppNumValue TagCustomRendered n = ppCustomRendered n
ppNumValue TagExposureMode n = ppTagExposureMode n
ppNumValue TagWhiteBalance n = ppTagWhiteBalance n
ppNumValue TagSceneCaptureType n = ppSceneCaptureType n
ppNumValue TagContrast n = ppTagContrastSharpness n
ppNumValue TagSaturation n = ppTagSaturation n
ppNumValue TagSharpness n = ppTagContrastSharpness n
ppNumValue _ n = show n

-- pretty print of tag Resolution Unit
ppResolutionUnit :: Int -> String
ppResolutionUnit 1 = "No absolute unit"
ppResolutionUnit 2 = "Inch"
ppResolutionUnit 3 = "Centimeter"
ppResolutionUnit n = undef n

-- pretty print of tag Orientation 
ppOrientation :: Int -> String
ppOrientation 1 = "Top-left"
ppOrientation 2 = "Top-right" 
ppOrientation 3 = "Bottom-right"
ppOrientation 4 = "Bottom-left"
ppOrientation 5 = "Left-top"
ppOrientation 6 = "Right-top"
ppOrientation 7 = "Right-bottom"
ppOrientation 8 = "Left-bottom"
ppOrientation n = undef n

--pretty print of tag YCbCrPositioning
ppYCbCrPositioning :: Int -> String
ppYCbCrPositioning 1 = "Centered"
ppYCbCrPositioning 2 = "Co-sited"
ppYCbCrPositioning n = undef n

-- pretty print of tag Compression 
ppCompression :: Int -> String
ppCompression 1 = "No compression"
ppCompression 2 = "CCITT modified Huffman RLE"
ppCompression 3 = "CCITT Group 3 fax"
ppCompression 4 = "CCITT Group 4 fax"
ppCompression 5 = "LZW"
ppCompression 6 = "JPEG compression"
ppCompression 7 = "JPEG (new style)"
ppCompression n = undef n

-- pretty print of tag ExposureProgram
ppTagExposureProgram :: Int -> String
ppTagExposureProgram 0 = "Not defined"
ppTagExposureProgram 1 = "Manual"
ppTagExposureProgram 2 = "Normal program"
ppTagExposureProgram 3 = "Aperture priority"
ppTagExposureProgram 4 = "Shutter priority"
ppTagExposureProgram 5 = "Creative program" -- (biased toward depth of field)
ppTagExposureProgram 6 = "Action program"   -- (biased toward fast shutter speed)
ppTagExposureProgram 7 = "Portrait mode"    -- (for closeup photos with the background out of focus)
ppTagExposureProgram 8 = "Landscape mode"   -- (for landscape photos with the background in focus)
ppTagExposureProgram n = undef n


ppTagMeteringMode :: Int -> String
ppTagMeteringMode 0 = "Unknown"
ppTagMeteringMode 1 = "Average"
ppTagMeteringMode 2 = "CenterWeightedAverage"
ppTagMeteringMode 3 = "Spot"
ppTagMeteringMode 4 = "MultiSpot"
ppTagMeteringMode 5 = "Pattern"
ppTagMeteringMode 6 = "Partial"
ppTagMeteringMode 255 = "other"
ppTagMeteringMode n = undef n

ppTagLightSource :: Int -> String
ppTagLightSource 0 = "Unknown"
ppTagLightSource 1 = "Daylight"
ppTagLightSource 2 = "Fluorescent"
ppTagLightSource 3 = "Tungsten (incandescent light)"
ppTagLightSource 4 = "Flash"
ppTagLightSource 9 = "Fine weather"
ppTagLightSource 10 = "Cloudy weather"
ppTagLightSource 11 = "Shade"
ppTagLightSource 12 = "Daylight fluorescent (D 5700 - 7100K)"
ppTagLightSource 13 = "Day white fluorescent (N 4600 - 5400K)"
ppTagLightSource 14 = "Cool white fluorescent (W 3900 - 4500K)"
ppTagLightSource 15 = "White fluorescent (WW 3200 - 3700K)"
ppTagLightSource 17 = "Standard light A"
ppTagLightSource 18 = "Standard light B"
ppTagLightSource 19 = "Standard light C"
ppTagLightSource 20 = "D55"
ppTagLightSource 21 = "D65"
ppTagLightSource 22 = "D75"
ppTagLightSource 23 = "D50"
ppTagLightSource 24 = "ISO studio tungsten"
ppTagLightSource 255 = "Other light source"
ppTagLightSource n = undef n


ppTagFlash :: Int -> String
ppTagFlash 0x0000 = "Flash did not fire"
ppTagFlash 0x0001 = "Flash fired"
ppTagFlash 0x0005 = "Strobe return light not detected"
ppTagFlash 0x0007 = "Strobe return light detected"
ppTagFlash 0x0009 = "Flash fired, compulsory flash mode"
ppTagFlash 0x000D = "Flash fired, compulsory flash mode, return light not detected"
ppTagFlash 0x000F = "Flash fired, compulsory flash mode, return light detected"
ppTagFlash 0x0010 = "Flash did not fire, compulsory flash mode"
ppTagFlash 0x0018 = "Flash did not fire, auto mode"
ppTagFlash 0x0019 = "Flash fired, auto mode"
ppTagFlash 0x001D = "Flash fired, auto mode, return light not detected"
ppTagFlash 0x001F = "Flash fired, auto mode, return light detected"
ppTagFlash 0x0020 = "No flash function"
ppTagFlash 0x0041 = "Flash fired, red-eye reduction mode"
ppTagFlash 0x0045 = "Flash fired, red-eye reduction mode, return light not detected"
ppTagFlash 0x0047 = "Flash fired, red-eye reduction mode, return light detected"
ppTagFlash 0x0049 = "Flash fired, compulsory flash mode, red-eye reduction mode"
ppTagFlash 0x004D = "Flash fired, compulsory flash mode, red-eye reduction mode, return light not detected"
ppTagFlash 0x004F = "Flash fired, compulsory flash mode, red-eye reduction mode, return light detected"
ppTagFlash 0x0059 = "Flash fired, auto mode, red-eye reduction mode"
ppTagFlash 0x005D = "Flash fired, auto mode, return light not detected, red-eye reduction mode"
ppTagFlash 0x005F = "Flash fired, auto mode, return light detected, red-eye reduction mode"
ppTagFlash n = undef n

ppTagColorSpace :: Int -> String
ppTagColorSpace 1 = "sRGB"
ppTagColorSpace 65535 = "Uncalibrated"
ppTagColorSpace n = undef n 

ppCustomRendered :: Int -> String
ppCustomRendered 0 = "Normal process"
ppCustomRendered 1 = "Custom process"
ppCustomRendered n = undef n

ppTagExposureMode :: Int -> String
ppTagExposureMode 0 = "Auto exposure"
ppTagExposureMode 1 = "Manual exposure"
ppTagExposureMode 2 = "Auto bracket"
ppTagExposureMode n = undef n

-- pretty print of tag WhiteBalance
ppTagWhiteBalance :: Int -> String
ppTagWhiteBalance 0 = "Auto white balance"
ppTagWhiteBalance 1 = "Manual white balance"
ppTagWhiteBalance n = undef n

-- pretty print of tag SceneCaptureType
ppSceneCaptureType :: Int -> String
ppSceneCaptureType 0 = "Standard"
ppSceneCaptureType 1 = "Landscape"
ppSceneCaptureType 2 = "Portrait"
ppSceneCaptureType 3 = "Night scene"
ppSceneCaptureType n = undef n

ppTagContrastSharpness :: Int -> String
ppTagContrastSharpness 0 = "Normal"
ppTagContrastSharpness 1 = "Soft"
ppTagContrastSharpness 2 = "Hard"
ppTagContrastSharpness n = undef n

ppTagSaturation :: Int -> String
ppTagSaturation 0 = "Normal"
ppTagSaturation 1 = "Low saturation"
ppTagSaturation 2 = "High saturation"
ppTagSaturation n = undef n

undef :: Int -> String
undef n = "undefined " ++  show n
