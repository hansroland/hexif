-- | Print out nicely the contents the data of IFDEntries.

{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Graphics.Hexif.PrettyPrint
      ( prettyPrint
      , ppIfdEntry
      ) where

import Graphics.Hexif.Types
import Graphics.Hexif.PrettyPrintInt
import Graphics.Hexif.PrettyPrintRat

import Text.Printf (printf)
import Data.List(intersperse)
import Data.Char (chr, ord)
import Data.Binary
import Data.Maybe
import qualified Data.Map as Map

-- | Pretty print all entries of the whole Ifd
prettyPrint :: Exif Ifd -> [PrettyEntry]
prettyPrint (Exif _ ifd) = concatMap prettyPrintIfd $ Map.assocs ifd

-- Pretty print an IFD including it's title
prettyPrintIfd :: (Word16, Ifd) -> [PrettyEntry]
prettyPrintIfd (ifdTag, ifdEntries) = title : entries
  where
    title = PrettyTitle $ fromMaybe "" mbTitle
    mbTitle = Map.lookup ifdTag  titlesMap
    entries = map ppIfdEntry $ Map.elems ifdEntries

-- A map containing the different IFD titles
titlesMap :: Map.Map Word16 String
titlesMap = Map.fromList [ (tag0thIfd, "0th IFD")
                         , (tagExifIfd, "Exif IFD")
                         , (tagGpsIfd,  "GPS IFD")
                         , (tagIntopIfd, "Interop IFD")
                         , (tag1stIfd, "1 st IFD (Thumbnail)")]


-- | Pretty print a single exif field
ppIfdEntry :: IfdEntry -> PrettyEntry
ppIfdEntry (IfdEntry tag val) =
    PrettyTag (drop 3 (show tag)) (ppIfdValue tag val)

-- PrettyPrint an ExifValue field
ppIfdValue :: ExifTag -> ExifValue -> String
ppIfdValue tag (ValueRat rats)       = ppRationalValues tag rats
ppIfdValue tag (ValueInt nVal)       = ppIntValue tag nVal
ppIfdValue tag (ValueStr strVal)     = ppStrValue tag strVal
ppIfdValue tag (ValueUdf len strVal) = ppUndefinedValue tag len strVal

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
ppStrValue TagGPSStatus strVal = ppTagGPSStatus strVal
ppStrValue TagGPSSpeedRef strVal = ppTagGPSSpeedRef strVal
ppStrValue TagGPSTrackRef strVal = ppTagGPSTrackRef strVal
ppStrValue TagGPSDestBearingRef strVal = ppTagGPSTrackRef strVal
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

-- | Pretty printer for the Scene type
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

ppTagGPSStatus :: String -> String
ppTagGPSStatus "A" = "Measurement Active"
ppTagGPSStatus "V" = "Measurement Void"
ppTagGPSStatus s   = "unknown " ++ s

ppTagGPSSpeedRef :: String -> String
ppTagGPSSpeedRef "K" = "km/h"
ppTagGPSSpeedRef "M" = "mph"
ppTagGPSSpeedRef "N" = "knots"
ppTagGPSSpeedRef s  = "unknown " ++ s

-- also used for ppTagGPSDestBearingRef
ppTagGPSTrackRef :: String -> String
ppTagGPSTrackRef "M" = "Magnetic North"
ppTagGPSTrackRef "T" = "True North"
ppTagGPSTrackRef s = "unknown " ++ s
