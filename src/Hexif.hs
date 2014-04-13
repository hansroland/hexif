-- -----------------------------------------------------------------------------
-- Hexif.hs
-- -----------------------------------------------------------------------------
--
-- Public functions for the hexif project
--
-- -----------------------------------------------------------------------------

module Hexif 
  where

import DataExif
import Reader
import PrettyPrint

import qualified Data.ByteString.Lazy as BL

-- Return a list of all ExifFields
allTags :: Exif -> [ExifField]
allTags exif = prettyPrint $ concat $ dirs exif

-- Return the exit data from a jpeg file
fromFile :: FilePath -> IO Exif
fromFile fn = do
   inp <- BL.readFile fn
   let input = BL.drop 6 inp
   return $ readExif input 

-- 
dirs :: Exif -> [IFDDir]
dirs (Exif dirs _) = dirs

