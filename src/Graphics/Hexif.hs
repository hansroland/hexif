{-|
     Read and interpret the exif file of a JPEG image only with Haskell code.

     This hexif library has similar functionality as the "exif" package (<http://hackage.haskell.org/package/exif-3000.0.0/docs/Graphics-Exif.html>). The exif package contains the bindings (wrapper) to the libexif C EXIF library (<http://libexif.sourceforge.net/>).

The first example shows how to print out all supported exif information of a JPEG image.

> processFile :: FilePath -> IO()
> processFile fn = do
>     exif <- fromFile fn
>     mapM_ print (allTags exif)
>
> -- processFile "RS4748.JPG"

The next example prints out the value of a single tag:

> singleTag :: FilePath -> ExifTag -> IO()
> singleTag fn tag = do
>     exif <- fromFile fn
>     print $ getTag exif tag
> 
> -- singleTag "RS4847.JPG" TagComponentsConfiguration


For more information about JPG and Exif, see

     *  <http://www.kodak.com/global/plugins/acrobat/en/service/digCam/exifStandard2.pdf>

     *  <http://www.media.mit.edu/pia/Research/deepview/exif.html>

     *  <http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif.html>
-}

module Graphics.Hexif 
  (ExifField(..)
  , ExifTag(..)
  , allTags
  , getTag
  , allEntries
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
    removeDebugs f = exifTag f `notElem` 
      [ TagSubDirIFDMain
      , TagSubDirIFDExif
      , TagSubDirIFDGPS
      , TagSubDirIFDInterop
      , TagOffsetSchemata
      ]

-- | Return the value of a single Exif tag.
getTag :: Exif -> ExifTag -> Maybe String
getTag exif tag = 
     if null tags 
         then Nothing
         else Just $ value $ head tags
     where tags = filter (\ef -> exifTag ef == tag) (allTags exif)

-- | Return a list of all ExifFields including the debug tags.
--   Do NOT use this function. It will be deleted later.
allTagsInclDebug :: Exif -> [ExifField]
allTagsInclDebug = prettyPrint . allEntries

-- | Return a list of our data entries
allEntries :: Exif -> IFDDataDir
allEntries exif = concat $ dirs exif

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


