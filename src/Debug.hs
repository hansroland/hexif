-- ----------------------------------------------------------------------------
-- Debug.hs
-- ----------------------------------------------------------------------------
-- 
-- Different debug functions for the hexif project.
--     Should help to find out what is going wrong
--
--
-- For debugging we always use the file produced with dumpExif
--
-- Move this file to the src directory of the hexif project
--     and use GHCi
--
-- ----------------------------------------------------------------------------

import Graphics.Hexif
import Graphics.Hexif.DataExif
import Graphics.Hexif.Reader
import Graphics.Hexif.Jpeg
import Graphics.Hexif.Utils

import System.FilePath
import Text.Printf (printf)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BL

main :: IO()
main = do
     args <- getArgs
     if null args
       then putStrLn "usage: ???? filename"
       else processFile $ head args

processFile :: FilePath -> IO()
processFile fn = do
    exif <- fromFile fn
    mapM_ print (allEntries exif)

instance Show IFDData where show = showRaw

showRaw :: IFDData -> String

showRaw (IFDRat tag fmt rats) = "IFDRat " ++ show tag ++ " " ++ show fmt ++ " " ++ show rats ++ "\n"
showRaw (IFDNum tag fmt n)    = "IFDNum " ++ show tag ++ " " ++ show fmt ++ " " ++ show n  ++ "\n"
showRaw (IFDStr tag fmt s)    = "IFDStr " ++ show tag ++ " " ++ show fmt ++ " " ++ s ++ "\n"
showRaw (IFDUdf tag fmt n s)  = "IFDStr " ++ show tag ++ " " ++ show fmt ++ " " ++ show n ++ (concat $ map show s) ++ "\n"
showRaw (IFDSub dirTag fmt dir) = "SUBDIRECTORY " ++ show dirTag ++ "\n" ++ (concat $ map showRaw dir) 

-- | Debug a special offset of a file
debugOffset :: IO()
debugOffset = do
      inp <- BL.readFile "BS0000.exif"
      let  (_, offset) = readHeader $ BL.drop 6 inp
      putStrLn $ show offset 


        
