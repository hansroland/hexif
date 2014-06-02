-- | Print out nicely the contents the data of IFDEntries.
-- This module is an internal module of Graphics.Hexif and should be used in the hexif project!

{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Graphics.Hexif.PrettyPrint (prettyPrint) where

import Graphics.Hexif.DataExif
import Graphics.Hexif.PrettyPrintInt
import Graphics.Hexif.PrettyPrintRat
import Text.Printf (printf)
import Data.List(intersperse)

import Data.Char (chr, ord)

-- | pretty print the contents all exif fields
prettyPrint :: DataBlock -> [ExifField]
prettyPrint entries = ppEntries entries ++ ppSubEntries entries

ppEntries :: DataBlock -> [ExifField]
ppEntries entries = map ppDataEntry (filter (not . isSubBlock) entries)

ppSubEntries :: DataBlock -> [ExifField]
ppSubEntries entries =
    ppEntries $ concatMap subBlock (filter isSubBlock entries)

subBlock :: DataEntry -> DataBlock
subBlock (DataSub _ b) = b
subBlock _ = []


isSubBlock :: DataEntry -> Bool
isSubBlock (DataSub _ _ ) = True
isSubBlock _ = False

-- | Pretty print a single exif field
ppDataEntry :: DataEntry -> ExifField
ppDataEntry (DataRat tag _ rats) = ExifField tag (ppRationalValues tag rats)
ppDataEntry (DataNum tag _ nVal) = ExifField tag (ppNumValue tag nVal)
ppDataEntry (DataStr tag _ strVal) = ExifField tag (ppStrValue tag strVal)
ppDataEntry (DataUdf tag _ len strVal) = ExifField tag (ppUndefinedValue tag len strVal)
ppDataEntry (DataSub tag _ ) = error $ "Directory encountered " ++ show tag

-- | Pretty printers for Undefined Values
ppUndefinedValue :: ExifTag -> Int -> String -> String
ppUndefinedValue TagExifVersion _ value = ppExifVersion value
ppUndefinedValue TagFlashPixVersion _ value = ppFlashPixVersion value
ppUndefinedValue TagComponentsConfiguration _ value = ppComponentsConfiguration value
ppUndefinedValue TagFileSource _ value = ppFileSource value
ppUndefinedValue TagSceneType _ value = ppSceneType value
ppUndefinedValue TagInteroperabilityVersion _ value = value
ppUndefinedValue _ len _ = show len ++ " bytes undefined data"

-- | PrettyPrinters for String values
ppStrValue :: ExifTag -> String -> String
ppStrValue TagXPAuthor strVal = removeNull strVal
ppStrValue TagXPTitle strVal = removeNull  strVal
ppStrValue TagGPSVersionID strVal = intersperse '.' strVal
ppStrValue TagGPSAltitudeRef strVal= ppTagGPSAltitudeRef strVal
ppStrValue _ strVal = rtrimX00 strVal

-- | Remove trailing hex zeros 0x00 from strings
rtrimX00 :: String -> String
rtrimX00 = reverse . dropWhile (\c -> ord c == 0) . reverse

-- | Little support function for ppStrValue
removeNull :: String -> String
removeNull = filter (\c -> ord c /= 00)
     
-- | Pretty printer for the Exif version
ppExifVersion :: String -> String
ppExifVersion value = "Exif Version " ++ show num
  where num :: Float = read value / 100.0

-- | Pretty printer for the flash pix value
ppFlashPixVersion :: String -> String
ppFlashPixVersion value = printf "FlashPix Version %.1f" num
  where num :: Float = read value / 100.0

-- | Pretty printer for the Components Configuration
ppComponentsConfiguration :: String -> String
ppComponentsConfiguration conf = unwords $ map ppComps conf
   where
       ppComps (ord -> 0)  = "-"
       ppComps (ord -> 1) = "Y"
       ppComps (ord -> 2) = "Cb"
       ppComps (ord -> 3) = "Cr"
       ppComps (ord -> 4) = "R"
       ppComps (ord -> 5) = "G"
       ppComps (ord -> 6) = "B"
       ppComps (ord -> _) = "??"

-- | Pretty printer for the file source
ppFileSource :: String -> String
ppFileSource value = 
      if head value == chr 3 
      then "DSC" 
      else "(unknown)"

-- |Pretty printer for the Scene type
ppSceneType :: String -> String
ppSceneType value = 
      if head value == chr 1 
      then "Directly photographed" 
      else "(unknown)"
   
-- | Pretty printer for the tag GPSAltitudeRef
ppTagGPSAltitudeRef :: String -> String
ppTagGPSAltitudeRef s = case s of
    "0" -> "Sea level"
    "1" -> "Below sea level"
    _ -> "unknown " ++ s

