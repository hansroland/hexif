-- | This module pretty prints all the exif values with an integer code.
-- This module is an internal module of Graphics.Hexif and should only be used in the hexif project!
module Graphics.Hexif.PrettyPrintInt where

import Graphics.Hexif.DataExif


-- | PrettyPrint functions for all Integer values.
ppNumValue :: ExifTag -> Int -> String
ppNumValue tg n = case tg of
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
    TagSensingMethod     -> ppTagSensingMethod n
    TagSubjectDistanceRange -> ppTagSubjectDistanceRange n
    TagFocalPlaneResolutionUnit -> ppFocalPlaneResolutionUnit n
    TagPhotometricInterpretation -> ppTagPhotometricInterpretation n
    _                    -> show n

-- | Pretty printer for the tag Resolution Unit.
ppResolutionUnit :: Int -> String
ppResolutionUnit n = case n of
    1 -> "No absolute unit"
    2 -> "Inch"
    3 -> "Centimeter"
    _ -> undef n

-- | Pretty print for the tag Orientation.
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

-- | Pretty printer for the tag YCbCrPositioning.
ppYCbCrPositioning :: Int -> String
ppYCbCrPositioning n = case n of
    1 -> "Centered"
    2 -> "Co-sited"
    _ -> undef n

-- | Pretty printer for the tag Compression.
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

-- | Pretty printer for the tag ExposureProgram.
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


-- | Pretty printer for the tag MeteringMode.
ppTagMeteringMode :: Int -> String
ppTagMeteringMode n = case n of
    0 -> "Unknown"
    1 -> "Average"
    2 -> "Center-weighted average"
    3 -> "Spot"
    4 -> "MultiSpot"
    5 -> "Pattern"
    6 -> "Partial"
    255 -> "other"
    _ -> undef n

-- | Pretty printer for the tag LightSource.
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

-- | Pretty printer for the tag Flash.
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

-- | Pretty printer for the tag ColorSpace.
ppTagColorSpace :: Int -> String
ppTagColorSpace n = case n of
    1     -> "sRGB"
    65535 -> "Uncalibrated"
    _     -> undef n 

-- | Pretty printer for the tag CustomRendered.
ppCustomRendered :: Int -> String
ppCustomRendered n = case n of
    0 -> "Normal process"
    1 -> "Custom process"
    _ -> undef n

-- | Pretty printer for the tag ExposureMode.
ppTagExposureMode :: Int -> String
ppTagExposureMode n = case n of
    0 -> "Auto exposure"
    1 -> "Manual exposure"
    2 -> "Auto bracket"
    _ -> undef n

-- | Pretty printer of tag WhiteBalance.
ppTagWhiteBalance :: Int -> String
ppTagWhiteBalance n = case n of 
    0 -> "Auto white balance"
    1 -> "Manual white balance"
    _ -> undef n

-- | Pretty printer of tag SceneCaptureType.
ppSceneCaptureType :: Int -> String
ppSceneCaptureType n = case n of
    0 -> "Standard"
    1 -> "Landscape"
    2 -> "Portrait"
    3 -> "Night scene"
    _ -> undef n

-- | Pretty printer for the tag GainControl.
ppTagGainControl :: Int -> String
ppTagGainControl n = case n of
    0 -> "Normal"
    1 -> "Low gain up"
    2 -> "High gain up"
    3 -> "Low gain down"
    4 -> "High gain down"
    _ -> undef n

-- | Pretty printer for the tag ContrastSharpness.
ppTagContrastSharpness :: Int -> String
ppTagContrastSharpness n = case n of
    0 -> "Normal"
    1 -> "Soft"
    2 -> "Hard"
    _ -> undef n

-- | Pretty printer for the tag Saturation.
ppTagSaturation :: Int -> String
ppTagSaturation n = case n of
    0 -> "Normal"
    1 -> "Low saturation"
    2 -> "High saturation"
    _ -> undef n

-- | Pretty printer for the tag SensingMethod
ppTagSensingMethod :: Int -> String
ppTagSensingMethod n = case n of
    1 -> "Not defined"
    2 -> "One-chip color area sensor"
    3 -> "Two-chip color area sensor"
    4 -> "Three-chip color area sensor"
    5 -> "Color sequential area sensor"
    7 -> "Trilinear sensor"
    8 -> "Color sequential linear sensor"
    _ -> undef n

-- | Pretty printer for the tag SucjectDistanceRange
ppTagSubjectDistanceRange :: Int-> String
ppTagSubjectDistanceRange n = case n of
    0 -> "Unknown"
    1 -> "Macro"
    2 -> "Close view"
    3 -> "Distant view"
    _ -> undef n

-- | Pretty printer for the tag FocalPlanResolutionUnit
ppFocalPlaneResolutionUnit :: Int -> String
ppFocalPlaneResolutionUnit n = case n of
    1 -> "No absolute unit of measurement"
    2 -> "Inch"
    3 -> "Centimeter"
    _ -> undef n

-- | Pretty printer for the tag PhotometricInterpretation
ppTagPhotometricInterpretation :: Int -> String
ppTagPhotometricInterpretation n = case n of
    0 -> "WhiteIsZero"
    1 -> "BlackIsZero"
    2 -> "RGB"
    3 -> "Palette color"
    4 -> "Transparency Mask"
    5 -> "Seperated, usually CMYK"
    6 -> "YCbCr"
    8 -> "CIE L*a*b*"
    9 -> "CIE L*a*b*"
    10 -> "CIE L*a*b*"
    32803 -> "CFA (Color Filter Array)"
    34892 -> "LinearRaw"
    _ -> undef n

-- | Report a tag we don't process properly. We don't yet know the tag!!
undef :: Int -> String
undef n = "undefined " ++  show n
