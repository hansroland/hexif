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

-- Note: subDirs will always be printed at the end
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
ppRationalValue TagMaxApertureValue r = ppMaxApertureValue r
ppRationalValue TagDigitalZoomRatio r = printf "%.2f" (rat2Float r)
ppRationalValue TagBrightnessValue r = ppBrightnessValue r
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

-- Pretty print the value of the tag MaxApertureValue
ppMaxApertureValue :: (Int, Int) -> String
ppMaxApertureValue r = printf "%.2f EV (f/%.1f)" f pf
  where 
    f = rat2Float r
    pf = 2 ** (f / 2)


ppBrightnessValue :: (Int, Int) -> String
ppBrightnessValue r = printf "%.2f EV (%.2f cd/m^2)" f pf
  where
    f = rat2Float r
    pf = 1 / (pi * 0.3048 * 0.3048) * 2 ** f

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
ppNumValue tag n = case tag of
    TagCompression       -> ppCompression n
    TagResolutionUnit    -> ppResolutionUnit n
    TagOrientation       -> ppOrientation n
    TagYCbCrPositioning  -> ppYCbCrPositioning n
    TagExposureProgram   -> ppTagExposureProgram n
    TagMeteringMode      -> ppTagMeteringMode n
    TagLightSource       -> ppTagLightSource n
    TagFlash             -> ppTagFlash n
    TagColorSpace        -> ppTagColorSpace n
    TagCustomRendered    -> ppCustomRendered n
    TagExposureMode      -> ppTagExposureMode n
    TagWhiteBalance      -> ppTagWhiteBalance n
    TagSceneCaptureType  -> ppSceneCaptureType n
    TagGainControl       -> ppTagGainControl n
    TagContrast          -> ppTagContrastSharpness n
    TagSaturation        -> ppTagSaturation n
    TagSharpness         -> ppTagContrastSharpness n
    _                    -> show n

-- pretty print of tag Resolution Unit
ppResolutionUnit :: Int -> String
ppResolutionUnit n = case n of
    1 -> "No absolute unit"
    2 -> "Inch"
    3 -> "Centimeter"
    _ -> undef n

-- pretty print of tag Orientation 
ppOrientation :: Int -> String
ppOrientation n = case n of
    1 -> "Top-left"
    2 -> "Top-right" 
    3 -> "Bottom-right"
    4 -> "Bottom-left"
    5 -> "Left-top"
    6 -> "Right-top"
    7 -> "Right-bottom"
    8 -> "Left-bottom"
    _ -> undef n

--pretty print of tag YCbCrPositioning
ppYCbCrPositioning :: Int -> String
ppYCbCrPositioning n = case n of
    1 -> "Centered"
    2 -> "Co-sited"
    _ -> undef n

-- pretty print of tag Compression 
ppCompression :: Int -> String
ppCompression n = case n of
    1 -> "No compression"
    2 -> "CCITT modified Huffman RLE"
    3 -> "CCITT Group 3 fax"
    4 -> "CCITT Group 4 fax"
    5 -> "LZW"
    6 -> "JPEG compression"
    7 -> "JPEG (new style)"
    _ -> undef n

-- pretty print of tag ExposureProgram
ppTagExposureProgram :: Int -> String
ppTagExposureProgram n = case n of
    0 -> "Not defined"
    1 -> "Manual"
    2 -> "Normal program"
    3 -> "Aperture priority"
    4 -> "Shutter priority"
    5 -> "Creative program (biased toward depth of field)"
    6 -> "Action program (biased toward fast shutter speed)"
    7 -> "Portrait mode (for closeup photos with the background out of focus)"
    8 -> "Landscape mode (for landscape photos with the background in focus)"
    _ -> undef n


ppTagMeteringMode :: Int -> String
ppTagMeteringMode n = case n of
    0 -> "Unknown"
    1 -> "Average"
    2 -> "CenterWeightedAverage"
    3 -> "Spot"
    4 -> "MultiSpot"
    5 -> "Pattern"
    6 -> "Partial"
    255 -> "other"
    _ -> undef n

ppTagLightSource :: Int -> String
ppTagLightSource n = case n of
    0 -> "Unknown"
    1 -> "Daylight"
    2 -> "Fluorescent"
    3 -> "Tungsten (incandescent light)"
    4 -> "Flash"
    9 -> "Fine weather"
    10 -> "Cloudy weather"
    11 -> "Shade"
    12 -> "Daylight fluorescent (D 5700 - 7100K)"
    13 -> "Day white fluorescent (N 4600 - 5400K)"
    14 -> "Cool white fluorescent (W 3900 - 4500K)"
    15 -> "White fluorescent (WW 3200 - 3700K)"
    17 -> "Standard light A"
    18 -> "Standard light B"
    19 -> "Standard light C"
    20 -> "D55"
    21 -> "D65"
    22 -> "D75"
    23 -> "D50"
    24 -> "ISO studio tungsten"
    255 -> "Other light source"
    _ -> undef n


ppTagFlash :: Int -> String
ppTagFlash n = case n of
    0x0000 -> "Flash did not fire"
    0x0001 -> "Flash fired"
    0x0005 -> "Strobe return light not detected"
    0x0007 -> "Strobe return light detected"
    0x0009 -> "Flash fired, compulsory flash mode"
    0x000D -> "Flash fired, compulsory flash mode, return light not detected"
    0x000F -> "Flash fired, compulsory flash mode, return light detected"
    0x0010 -> "Flash did not fire, compulsory flash mode"
    0x0018 -> "Flash did not fire, auto mode"
    0x0019 -> "Flash fired, auto mode"
    0x001D -> "Flash fired, auto mode, return light not detected"
    0x001F -> "Flash fired, auto mode, return light detected"
    0x0020 -> "No flash function"
    0x0041 -> "Flash fired, red-eye reduction mode"
    0x0045 -> "Flash fired, red-eye reduction mode, return light not detected"
    0x0047 -> "Flash fired, red-eye reduction mode, return light detected"
    0x0049 -> "Flash fired, compulsory flash mode, red-eye reduction mode"
    0x004D -> "Flash fired, compulsory flash mode, red-eye reduction mode, return light not detected"
    0x004F -> "Flash fired, compulsory flash mode, red-eye reduction mode, return light detected"
    0x0059 -> "Flash fired, auto mode, red-eye reduction mode"
    0x005D -> "Flash fired, auto mode, return light not detected, red-eye reduction mode"
    0x005F -> "Flash fired, auto mode, return light detected, red-eye reduction mode"
    _      -> undef n

ppTagColorSpace :: Int -> String
ppTagColorSpace n = case n of
    1     -> "sRGB"
    65535 -> "Uncalibrated"
    _     -> undef n 

ppCustomRendered :: Int -> String
ppCustomRendered n = case n of
    0 -> "Normal process"
    1 -> "Custom process"
    _ -> undef n

ppTagExposureMode :: Int -> String
ppTagExposureMode n = case n of
    0 -> "Auto exposure"
    1 -> "Manual exposure"
    2 -> "Auto bracket"
    _ -> undef n

-- pretty print of tag WhiteBalance
ppTagWhiteBalance :: Int -> String
ppTagWhiteBalance n = case n of 
    0 -> "Auto white balance"
    1 -> "Manual white balance"
    _ -> undef n

-- pretty print of tag SceneCaptureType
ppSceneCaptureType :: Int -> String
ppSceneCaptureType n = case n of
    0 -> "Standard"
    1 -> "Landscape"
    2 -> "Portrait"
    3 -> "Night scene"
    _ -> undef n

ppTagGainControl :: Int -> String
ppTagGainControl n = case n of
    0 -> "Normal"
    1 -> "Low gain up"
    2 -> "High gain up"
    3 -> "Low gain down"
    4 -> "High gain down"
    _ -> undef n

ppTagContrastSharpness :: Int -> String
ppTagContrastSharpness n = case n of
    0 -> "Normal"
    1 -> "Soft"
    2 -> "Hard"
    _ -> undef n

ppTagSaturation :: Int -> String
ppTagSaturation n = case n of
    0 -> "Normal"
    1 -> "Low saturation"
    2 -> "High saturation"
    _ -> undef n

undef :: Int -> String
undef n = "undefined " ++  show n
