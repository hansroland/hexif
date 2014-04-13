-- -----------------------------------------------------------------------------
-- Hexif.hs
-- -----------------------------------------------------------------------------
--
-- Public functions for the hexif project
--
-- Read (and maybe later rewrite) the exif file of a JPEG image with 
--     native Haskell code
--
-- See: 
--    http://www.media.mit.edu/pia/Research/deepview/exif.html
--    http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif.html   
--
--    
--
-- -----------------------------------------------------------------------------
module Hexif 
  where

import DataExif
import Jpeg
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

