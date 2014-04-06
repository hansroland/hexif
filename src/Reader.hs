-- -----------------------------------------------------------------------------
-- Reader.hs Functions to read in an Exif structure
-- -----------------------------------------------------------------------------

module Reader where

import DataExif

import Data.Char (chr)

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

type GetWords = (Get Word16, Get Word32)

type Offset = Int

readExif :: BL.ByteString -> [IFDFileDir]
readExif = runGet getExif 

-- Main function to read in the exif data structure
getExif :: Get [IFDFileDir]
getExif = do
    -- Tiff Header 
    tiffAlign <- getWord16be
    let getWords = if fromIntegral tiffAlign == 0x4949
        then (getWord16le, getWord32le)
        else (getWord16be, getWord32be) 
    const2A <- getByteString 2
    -- Get main directory
    wOffsetMain <- snd getWords
    mainBlock <- getIFDBlock (fromIntegral wOffsetMain) getWords
    let mainDir = IFDFileDir IFDMain mainBlock
    -- Get sub directory
    wOffsetSub <- snd getWords
    let nOffsetSub = fromIntegral wOffsetSub
    if fromIntegral nOffsetSub == 0 
       then return $ mainDir : []
       else do
           subBlock <- getIFDBlock nOffsetSub getWords
           let subDir = IFDFileDir IFDExif subBlock
           return $ mainDir : subDir : []

-- read a single IFD block. It contains n IFD entries. 
getIFDBlock :: Offset -> GetWords -> Get [IFDFileEntry]
getIFDBlock n getWords =
    if n == 0 
        then return []
        else do
           entry <- getIFDFileEntry getWords
           entries <- getIFDBlock (n - 1) getWords
           return $ entry : entries

-- read a single IFD file entry
getIFDFileEntry ::  GetWords -> Get IFDFileEntry
getIFDFileEntry (getWord16, getWord32)  = do
    tagNr <- getWord16
    format <- getWord16
    comps <- getWord32
    strBsValue <- getLazyByteString 4
    let offset = runGet getWord32 strBsValue
    return $ IFDFileEntry tagNr format (fromIntegral comps) (fromIntegral offset) (unpackLazyBS strBsValue)



-- little support functions: normal pack/unpack are refused by GHC
-- Hoogle says: unpack :: BL.ByteString -> String
-- GHCi says:   unpack ::  BL.ByteString -> [Word8]
-- Why is Haskell string conversion so difficult?
unpackLazyBS :: BL.ByteString -> String
unpackLazyBS = map (chr . fromIntegral)  . BL.unpack

