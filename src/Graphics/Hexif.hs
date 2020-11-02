module Graphics.Hexif
    ( Hexif()
    , PrettyEntry(..)
    , ExifTag(..)
    , fromFile
    , prettyPrint
    , getTag
    , findTag
    )

where

import Graphics.Hexif.Jpeg
import Graphics.Hexif.Types
import Graphics.Hexif.Parser
import Graphics.Hexif.Convert
import Graphics.Hexif.PrettyPrint
import Graphics.Hexif.Utils

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.List (find)

type Hexif = Exif Ifd


-- | Return the value of a single Exif tag.
getTag :: Hexif -> ExifTag -> Maybe PrettyEntry
getTag exif tag = ppIfdEntry <$> findTag exif tag

-- | Retun the IfdEntry of a single Exif tag
findTag :: Hexif -> ExifTag -> Maybe IfdEntry
findTag exif tag = find (\e -> entryTag e == tag) ifdEntries
   where
    ifdEntries = concatMap Map.elems $ Map.elems $ ifdMap exif

fromFile :: FilePath -> IO (Exif Ifd)
fromFile filepath = do
    etJpeg <- readJpegFromFile filepath
    case etJpeg of
      Left str -> do
        putStrLn str
        return $ Exif Intel Map.empty
      Right jpeg -> do
        let exifbs = extractExifOld jpeg
        let rawExif = parseRawExif exifbs
        let exif = convert exifbs rawExif
        return exif
        -- print exif
        -- print $ prettyPrint exif


-- | Extract the Exif segment from a JPEG value
-- TODO: Check for Exif constant at the beginning
extractExif :: Jpeg -> Either String BL.ByteString
extractExif jpeg = existsExif exifSegs
  where
    exifSegs = filter (\seg -> segMarker seg == 0xFFE1) (segments jpeg)
    existsExif [] = Left "No JPEG segments found"
    existsExif segs = Right $ BL.drop 6 $ segData $ head segs

-- | Extract the Exif segment from a JPEG value
-- TODO: Check for Exif constant at the beginning
extractExifOld :: Jpeg -> BL.ByteString
extractExifOld jpeg = existsExif
  where
    segs = filter (\seg -> segMarker seg == 0xFFE1) (segments jpeg)
    existsExif = BL.drop 6 $ segData $ head segs
