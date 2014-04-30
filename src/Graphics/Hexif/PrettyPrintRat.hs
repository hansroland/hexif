-- | This module pretty prints all the exif fields with rational values.
-- This module is an internal module of Graphics.Hexif and should only be used in the hexif project!

module Graphics.Hexif.PrettyPrintRat where

import Graphics.Hexif.DataExif
import Text.Printf (printf)

-- | pretty printer for exif tags with multiple rational values.
ppRationalValues :: ExifTag -> [(Int,Int)] -> String
ppRationalValues tag []       = "No values"
ppRationalValues tag (r : []) = ppRationalValue tag r
ppRationalValues TagGPSLatitude rs      = ppGPSLongLatt rs
ppRationalValues TagGPSLongitude rs     = ppGPSLongLatt rs
ppRationalValues TagGPSDestLatitude rs  = ppGPSLongLatt rs
ppRationalValues TagGPSDestLongitude rs = ppGPSLongLatt rs
ppRationalValues tag rs     = concatMap fmtRat' rs
    where fmtRat' r = fmtRat r ++ " "

-- | pretty printer for exif tags with a single rationalvalue.
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

-- | Helper function: Convert a rational to a float.
rat2Float :: (Int,Int) -> Float
rat2Float (n,d) = (fromIntegral n::Float) / (fromIntegral d:: Float)


-- | Helper function: Format a rational number with a slash.
fmtRatWithSlash :: (Int, Int) -> String
fmtRatWithSlash (num,denum) =
    show (div num ggt) ++ "/" ++ show (div denum ggt)
    where ggt = gcd num denum

-- | Format a rational number.
fmtRat :: (Int, Int) -> String
fmtRat r@(num, denum) = 
     if mod num denum == 0 then fmtRatInt r else fmtRatFloat r

-- | Format a rational number as an integer
fmtRatInt :: (Int, Int) -> String
fmtRatInt (num, denum) = show $ div num denum
 
-- | Format a rational number as a float.
fmtRatFloat :: (Int, Int) -> String
fmtRatFloat = show . rat2Float

-- | Pretty print the value of the tag ExposureBiasValue.
ppExposureBiasValue :: (Int, Int) -> String
ppExposureBiasValue r = printf "%.2f EV" (rat2Float r)

-- | Pretty print the value of the tag FocalLength.
ppFocalLength :: (Int, Int) -> String
ppFocalLength r = printf "%.1f mm" (rat2Float r)

-- | Pretty print the value of the tag MaxApertureValue.
ppMaxApertureValue :: (Int, Int) -> String
ppMaxApertureValue r = printf "%.2f EV (f/%.1f)" f pf
  where 
    f = rat2Float r
    pf = 2 ** (f / 2)

-- | Pretty print the value of the tag BightnessValue.
ppBrightnessValue :: (Int, Int) -> String
ppBrightnessValue r = printf "%.2f EV (%.2f cd/m^2)" f pf
  where
    f = rat2Float r
    pf = 1 / (pi * 0.3048 * 0.3048) * 2 ** f

-- | Pretty print the values for the latitude and longitude GPS fields.
ppGPSLongLatt :: [(Int,Int)] -> String
ppGPSLongLatt rs = fmtLL $ map rat2Float rs
  where 
    fmtLL rs@(d:m:s:[]) = printf "%.0f, %.0f, %.4f" d m s
    fmt  _ = "verify data format"

