-- | Module to read a jpeg file and split it into segments.
-- This module is an internal module of Graphics.Hexif and should only be used in the hexif project!
module Graphics.Hexif.Jpeg 
  ( Jpeg
  , readJpegFromFile
  , extractExif
) where

import Data.Binary
import Data.Binary.Get   {-( Get
                      , getWord8
                      , getWord16be
                      , getByteString
                      , skip
                      , bytesRead
                      )   -}

import qualified Data.ByteString.Lazy as BL

-- | A JPEG file is a list of so called segments
data Jpeg = Jpeg
    { segments :: [JpegSegment]
 -- , imageData :: B.ByteString      -- later needed for write version
    } 
    deriving Eq

-- | A segment contains an identifying marker and a length
data JpegSegment = JpegSegment
    { segMarker :: Int 
    , segData :: BL.ByteString 
    -- , offset :: Integer              -- for debugging only
    }
    deriving (Eq)
  
-- | Read a Jpeg value from a file
readJpegFromFile :: String -> IO Jpeg
readJpegFromFile fn = do
   bsJpeg <- BL.readFile fn
   return $ readJpeg bsJpeg

-- | Extract the Exif segment from a JPEG value
extractExif :: Jpeg -> BL.ByteString
extractExif jpeg = segData $ head (filter (\seg -> segMarker seg == 0xFFE1) (segments jpeg))

-- | Read a Jpeg value form a lazy ByteString
readJpeg :: BL.ByteString -> Jpeg
readJpeg = runGet getJpeg
  where
    getJpeg :: Get Jpeg
    getJpeg = do
        _ <- getWord16be
        segs <- getSegments 
        return $ Jpeg segs 

-- | get all the segments
getSegments :: Get [JpegSegment]
getSegments = do
    seg <- getSegment
    if segMarker seg == 0xFFDA
        then return [seg]
        else do 
          segs <- getSegments
          return $ seg : segs

-- | get the next segment
getSegment :: Get JpegSegment
getSegment = do
     marker <- getWord16be
     _ <- bytesRead
     len <- getWord16be
     sgData <- getLazyByteString (fromIntegral len - 2)
     return $ JpegSegment (fromIntegral marker) sgData
