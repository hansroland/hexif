-- | This module pretty prints all the exif fields with rational values.

module Graphics.Hexif.PrettyPrintRat
  ( ppRationalValues
  ) where

import Graphics.Hexif.Types

import Text.Printf (printf)
import GHC.Float


-- | Pretty printer for exif tags with multiple rational values.
ppRationalValues :: ExifTag -> [(Int,Int)] -> String
ppRationalValues _ []       = "No values"
ppRationalValues tg (r : []) = ppRationalValue tg r
ppRationalValues TagGPSLatitude rs      = ppGPSLongLatt rs
ppRationalValues TagGPSLongitude rs     = ppGPSLongLatt rs
ppRationalValues TagGPSDestLatitude rs  = ppGPSLongLatt rs
ppRationalValues TagGPSDestLongitude rs = ppGPSLongLatt rs
ppRationalValues TagGPSTimeStamp rs     = ppGPSTimeStamp $ map rat2Double rs
ppRationalValues TagGPS0a _             = "GPS Tag 0a"
ppRationalValues TagGPS0b _             = "GPS Tag 0b"
ppRationalValues TagGPS0f _             = "GPS Tag 0f"
ppRationalValues _ rs     = concatMap fmtRat' rs
    where fmtRat' r = fmtRat r ++ " "

-- | Pretty printer for exif tags with a single rational value.
ppRationalValue :: ExifTag -> (Int,Int) -> String
ppRationalValue t r
    | t == TagExposureTime = fmtRatWithSlash r ++ " sec."
    | t == TagFNumber = "f/" ++ fmtRatFloat r
    | t == TagCompressedBitsPerPixel = ' ' : fmtRat r
    | t == TagExposureBiasValue = ppExposureBiasValue r
    | t == TagFocalLength = ppFocalLength r
    | t == TagApertureValue = ppApertureValue f
    | t == TagMaxApertureValue = ppApertureValue f
    | t == TagShutterSpeedValue = ppShutterSpeedValue f
    | t == TagDigitalZoomRatio = printf "%.4f" f
    | t == TagBrightnessValue = ppBrightnessValue f
    | otherwise = fmtRat r
    where f = rat2Float r

-- | Helper function: Convert a rational to a float.
rat2Float :: (Int,Int) -> Float
rat2Float (n,d) = (fromIntegral n::Float) / (fromIntegral d::Float)

-- | Helper function: Convert a rational to a double.
rat2Double :: (Int,Int) -> Double
rat2Double (n,d) = (fromIntegral n::Double) / (fromIntegral d::Double)

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

-- | Pretty print the value of the tags ApertureValue and MaxApertureValue.
ppApertureValue :: Float -> String
ppApertureValue f = printf "%.2f EV (f/%.1f)" f pf
  where
    pf = 2 ** (f / 2)

-- | Pretty print the value of the tag ShutterSpeedValue.
ppShutterSpeedValue :: Float -> String
ppShutterSpeedValue f = printf "%.02f EV (1/%d sec.)" f (d::Int)
  where
    d = floor $ fromRational 2 ** f;

-- | Pretty print the value of the tag BightnessValue.
ppBrightnessValue :: Float -> String
ppBrightnessValue f = printf "%.2f EV (%.2f cd/m^2)" f pf
  where
    pf = 1 / (pi * 0.3048 * 0.3048) * 2 ** f

-- | Pretty print the values for the latitude and longitude GPS fields.
ppGPSLongLatt :: [(Int,Int)] -> String
ppGPSLongLatt rs = fmtLL fs
  where
    fs = map rat2Double rs
    fmtLL (r1:r2:r3:[]) = printf "%2d, %2d, %.4f" d m s
      where
        (d,m,s)  = degNorm r1 r2 r3
    fmtLL  _ = "verify data format"

-- | Support function for ppGPSLongLat: Normalize degrees
degNorm :: Double -> Double -> Double -> (Int, Int, Float)
degNorm dd mm ss = (d, m, s)
   where
     secs = dd * 3600 + mm * 60 + ss
     q1 = secs / 3600
     d = floor q1
     r1 = (q1 - fromIntegral d) * 60
     m = floor r1
     s = double2Float (r1 - fromIntegral m) * 60

-- | Pretty print GPS time stamp
ppGPSTimeStamp :: [Double] -> String
ppGPSTimeStamp [h, m, s] = printf "%02.0f:%02.0f:%05.2f" h m s
ppGPSTimeStamp _         = "Invalid date format"
