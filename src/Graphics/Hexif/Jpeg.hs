-- | Module to read a jpeg file and split it into segments.
-- See: https://docs.fileformat.com/image/jpeg/
module Graphics.Hexif.Jpeg
  ( Jpeg(..)
  , JpegSegment(..)
  , readJpegFromFile
) where

import Graphics.Hexif.Utils

import Data.Binary ( Word16, Get )
import Data.Binary.Get ( getLazyByteString, getWord16be )
import qualified Data.ByteString.Lazy as BL
import Data.Int ( Int64 )

-- | A JPEG file is a list of so called Jpeg segments
data Jpeg = Jpeg
    { segments :: [JpegSegment] }
    deriving Eq

-- | A segment contains an identifying marker and data
data JpegSegment = JpegSegment
    { segMarker :: Word16
    , segData :: BL.ByteString
    }
    deriving (Eq)

instance Show JpegSegment where
  show seg = "\nNew Segment " ++ showHex (segMarker seg)

-- | Read a Jpeg value from a file
readJpegFromFile :: String -> IO (Either String Jpeg)
readJpegFromFile fn = do
   -- putStrLn fn
   bsJpeg <- BL.readFile fn
   pure $ readJpeg bsJpeg

-- | Read a Jpeg value form a lazy ByteString
readJpeg :: BL.ByteString -> Either String Jpeg
readJpeg = runGetEither getJpeg
  where
    getJpeg :: Get Jpeg
    getJpeg = do
        _ <- getWord16be
        segs <- getSegments
        pure $ Jpeg segs

-- | Get all the segments
-- Note: This will fail if the EOI marker is not available !!
-- TODO: Avoid exception !!!
getSegments :: Get [JpegSegment]
getSegments = do
    seg <- getSegment
    -- Check for last marker EOI
    if segMarker seg == 0xFFDA
        then pure [seg]
        else do
          segs <- getSegments
          pure $ seg : segs

-- | Get the next segment
getSegment :: Get JpegSegment
getSegment = do
     marker <- getWord16be
     len <- getSegmentDataLength marker
     sgData <- getLazyByteString len
     pure $ JpegSegment marker sgData

-- Get data length of segment excluding length info
getSegmentDataLength :: Word16 -> Get Int64
getSegmentDataLength marker
 | marker == 0xFFD8 = pure 0                       -- SOI Start of Image
 | marker == 0xFFD9 = pure 0                       -- EOI End of Image
 | marker == 0xFFDD = pure 4                       -- DRI Define restart intervall
 | marker >= 0xFFD0 && marker <= 0xFFD7 = pure 0   -- RSTn Restart
 | otherwise        = do
     len <- getWord16be
     pure $ (fromIntegral len) - 2
