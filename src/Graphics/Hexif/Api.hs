module Graphics.Hexif.Api

where

import Graphics.Hexif
import Graphics.Hexif.Types
import Graphics.Hexif.Utils

import Data.Time ( defaultTimeLocale, parseTimeM, UTCTime )

-- | Return the value of the tag DateTimeOriginal,
--   if this is Nothing return the DateTimeModified.
getDateTime :: Hexif -> Maybe UTCTime
getDateTime exif = mbEntryValue >>=
    exifValueString >>=
    parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S"
  where
    mbEntryValue = entryValue <$>
      (findTag exif TagDateTimeOriginal) `ifNothing` findTag exif TagDateTime
