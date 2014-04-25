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
--    http://search.cpan.org/~rjbs/Image-MetaData-JPEG-0.155/lib/Image/MetaData/JPEG.pod
--    http://www.kodak.com/global/plugins/acrobat/en/service/digCam/exifStandard2.pdf
--
--
--
-- -----------------------------------------------------------------------------
module Graphics.Hexif 
  (ExifField(..)
  , ExifTag(..)
  , allTags
  , allTagsInclDebug
  , fromFile
  , fromExifFile
  , dumpExif)
  where

import Graphics.Hexif.DataExif
import Graphics.Hexif.Jpeg
import Graphics.Hexif.Reader
import Graphics.Hexif.PrettyPrint

import qualified Data.ByteString.Lazy as BL
import System.FilePath

-- Return a list of all ExifFields (but without debug tags)
allTags :: Exif -> [ExifField]
allTags exif = filter removeDebugs (allTagsInclDebug exif)
  where
    removeDebugs f = not $ elem (exifTag f) 
      [ TagSubDir_IFDMain
      , TagSubDir_IFDExif
      , TagSubDir_IFDGPS
      , TagSubDir_IFDInterop
      ]

-- Return a list of all ExifFields including the debug tags
allTagsInclDebug :: Exif -> [ExifField]
allTagsInclDebug exif = prettyPrint $ concat $ dirs exif

-- Return the exit data from a jpeg file
fromFile :: FilePath -> IO Exif
fromFile fn = do
    jpeg <- readJpegFromFile fn
    let bsExif = extractExif jpeg
    return $ readExif bsExif


-- Helper function to read exif data from a dumped exif file
fromExifFile :: FilePath -> IO Exif
fromExifFile fn = do
   inp <- BL.readFile fn
   return $ readExif inp 

-- Little getter function to extract the directories from an Exif value
dirs :: Exif -> [IFDDir]
dirs (Exif dirs _) = dirs

-- Debugging function: Write the Exif file separatly to disk
dumpExif :: FilePath -> IO ()
dumpExif fn = do 
    jpeg <- readJpegFromFile fn
    let newName = replaceExtension fn ".exif"
    BL.writeFile newName (extractExif jpeg)


