-- | Print out nicely the contents the data of IFDEntries

{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Graphics.Hexif.PrettyPrint (prettyPrint) where

import Graphics.Hexif.DataExif
import Graphics.Hexif.Utils
import Graphics.Hexif.PrettyPrintInt
import Graphics.Hexif.PrettyPrintRat
import Text.Printf (printf)

import Data.Char (chr, ord)

-- | pretty print the contents all exif fields 
prettyPrint :: IFDDataDir -> [ExifField]
prettyPrint entries = map ppIFDData (flatten entries)

-- | flaten out the tree of the directories
-- Note: subDirs will always be printed at the end
flatten :: IFDDataDir -> [IFDData]
flatten [] = []
flatten ((IFDSub tag ifdDir) : ds) = flatten ds ++ flatten ifdDir
flatten (d : ds) = d : flatten ds

-- | Pretty print a single exif field
ppIFDData :: IFDData -> ExifField 
ppIFDData (IFDRat tag rats) = ExifField tag (ppRationalValues tag rats)
ppIFDData (IFDNum tag nVal) = ExifField tag (ppNumValue tag nVal)
ppIFDData (IFDStr tag strVal) = ExifField tag strVal
ppIFDData (IFDUdf tag len strVal) = ExifField tag (ppUndefinedValue tag len strVal)
ppIFDData (IFDSub tag ifdDir) = error $ "Directory encountered " ++ show tag

-- | Pretty printers for Undefined Values
ppUndefinedValue :: ExifTag -> Int -> String -> String
ppUndefinedValue TagExifVersion len value = ppExifVersion value
ppUndefinedValue TagFlashPixVersion len value = ppFlashPixVersion value
ppUndefinedValue TagComponentsConfiguration len value = ppComponentsConfiguration value
ppUndefinedValue TagFileSource len value = ppFileSource value
ppUndefinedValue TagSceneType len value = ppScreenType value
ppUndefinedValue TagInteroperabilityVersion len value = value
ppUndefinedValue _ len _ = show len ++ " bytes undefined data"

-- | Pretty printer for the Exif version
ppExifVersion :: String -> String
ppExifVersion value = "Exif Version " ++ show num
  where num :: Float = read value / 100.0

-- | Pretty printer for the flash pix value
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
   

