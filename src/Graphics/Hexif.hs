{-|
     Read (and maybe later rewrite) the exif file of a JPEG image with native Haskell code

     This module has similar functionality as the "exif" package (<http://hackage.haskell.org/package/exif-3000.0.0/docs/Graphics-Exif.html>)

     For more information about JPG and Exif, see

     *  <http://www.kodak.com/global/plugins/acrobat/en/service/digCam/exifStandard2.pdf>

     *  <http://www.media.mit.edu/pia/Research/deepview/exif.html>

     *  <http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif.html>
-}

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

-- | Return a list of all ExifFields (but without debug tags).
allTags :: Exif -> [ExifField]
allTags exif = filter removeDebugs (allTagsInclDebug exif)
  where
    removeDebugs f = not $ elem (exifTag f) 
      [ TagSubDir_IFDMain
      , TagSubDir_IFDExif
      , TagSubDir_IFDGPS
      , TagSubDir_IFDInterop
      ]

-- | Return a list of all ExifFields including the debug tags.
--   Do NOT use this function. It will be deleted later.
allTagsInclDebug :: Exif -> [ExifField]
allTagsInclDebug exif = prettyPrint $ concat $ dirs exif

-- | Return the exit data from a jpeg file.
--   Use this function to initialize your exif value
fromFile :: FilePath -> IO Exif
fromFile fn = do
    jpeg <- readJpegFromFile fn
    let bsExif = extractExif jpeg
    return $ readExif bsExif


-- | Little getter function to extract the directories from an Exif value
dirs :: Exif -> [IFDDataDir]
dirs (Exif dirs _) = dirs

-- | Debugging function: Write the Exif file separatly to disk
--   Do not use this function. It's mainly used for debugging
dumpExif :: FilePath -> IO ()
dumpExif fn = do 
    jpeg <- readJpegFromFile fn
    let newName = replaceExtension fn ".exif"
    BL.writeFile newName (extractExif jpeg)

-- | Helper function to read exif data from a dumped exif file
--   Do not use this function. It's mainly used for debugging
fromExifFile :: FilePath -> IO Exif
fromExifFile fn = do
   inp <- BL.readFile fn
   return $ readExif inp 


