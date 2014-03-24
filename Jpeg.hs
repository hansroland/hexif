-- ----------------------------------------------------------------------------
-- ReadJpeg - Read a jpeg file and split it into segments
-- ----------------------------------------------------------------------------
import Data.Binary
import Data.Binary.Get   {-( Get
                      , getWord8
                      , getWord16be
                      , getByteString
                      , skip
                      , bytesRead
                      )   -}
-- import Data.Binary.Put
import Control.Monad
import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Hexdump (prettyHex)
import Numeric (showHex)

-- -----------------------------------------------------------------------------------
-- Data Types and simple instances
-- -----------------------------------------------------------------------------------
data Jpeg = Jpeg
    { segments :: [JpegSegment]
 -- , imageData :: B.ByteString      -- later needed for write version
    } 
    deriving (Eq, Show)

data JpegSegment = JpegSegment
    { segMarker :: Int 
    , segLen :: Int
    , segData :: B.ByteString 
    , offset :: Integer              -- for debugging only
    }
    deriving (Eq)

instance Show JpegSegment 
  where
    show = showSegment


showSegment :: JpegSegment -> String
showSegment seg =
   "JpegSegment: Marker = " ++ showHex (segMarker seg)  "" ++ " len: " ++ show (segLen seg) ++ 
   " offset = " ++ show (offset seg) ++
   " data   = " ++ prettyHex (B.take 32 $segData seg)
  
  
-- read the next segment
getSegment :: Get JpegSegment
getSegment = do
     marker <- getWord16be
     offset <- bytesRead
     len <- getWord16be 
     segData <- getByteString (fromIntegral len - 2)
     return $ JpegSegment (fromIntegral marker) (fromIntegral len) segData (fromIntegral offset - 2)


-- read all the segments
getSegments :: Get [JpegSegment]
getSegments = do
    seg <- getSegment
    if segMarker seg == 0xFFDA
        then return [seg]
        else do 
          segs <- getSegments
          return $ seg : segs


-- get a Jpeg value form a ByteSring         
getJpeg :: Get Jpeg
getJpeg = do
    jpegStart <- getWord16be
    segs <- getSegments 
    return $ Jpeg segs 

readJpeg :: BL.ByteString -> Jpeg
readJpeg bytes =
    runGet getJpeg bytes


example3 :: IO()
example3 = do
    input <- BL.readFile "JG1111.jpg"
    print $ readJpeg input

{-
example4 :: IO()
example4 = do
    input <- BL.readFile "JG1111.jpg"
    let res = runGet readJpeg input
    let exif = head (filter (\seg -> intSegMarker seg == 0xFFE1) res)
    B.writeFile "JG1111.exif" $ segData exif
-}    












