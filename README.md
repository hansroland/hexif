### Read the EXIF File of a JPEG image with Haskell Code only.

This *hexif* library has similar functionality as the [Hackage *exif* package](http://hackage.haskell.org/package/exif-3000.0.0/docs/Graphics-Exif.html). *Hexif* uses only pure Haskell code and no operating system library.

The first example shows how to print out all supported exif information of a JPEG image.

    processFile :: FilePath -> IO()
    processFile fn = do
        exif <- fromFile fn
        mapM_ putStrLn $ map format $ allFields exif
      where
        format (PrettyTitle t) = "\n" ++ t
        format (PrettyTag t v) = "  " ++ t ++ " -> " ++ v
    -- Example: processFile "RS4748.JPG"

The next example prints out the value of a single tag:

    singleTag :: FilePath -> ExifTag -> IO()
    singleTag fn tag = do
        exif <- fromFile fn
        print $ getTag exif tag
    -- Example: singleTag "RS4847.JPG" TagComponentsConfiguration

The supported exit tags are listed in the module Graphics.Hexif.DataExif.

Sample output of first example:

    0th IFD
      ImageDescription -> SONY DSC
      Model -> DSLR-A200
      Make -> SONY
      XResolution -> 72
      YResolution -> 72
      ResolutionUnit -> Inch
      Software -> DSLR-A200 v1.00
      DateTime -> 2012:08:12 13:14:44
      Artist -> Picasa
      YCbCrPositioning -> Co-sited
      PrintImageMatching -> 124 bytes undefined data

    Exif IFD
      ExposureTime -> 1/200 sec.
      FNumber -> f/10.0
      ExposureProgram -> Landscape mode (for landscape photos with the background in focus)
      ISOSpeedRatings -> 100
      ExifVersion -> Exif Version 2.21
      DateTimeOriginal -> 2012:08:10 13:08:57
      DateTimeDigitized -> 2012:08:10 13:08:57
      ComponentsConfiguration -> Y Cb Cr -
      CompressedBitsPerPixel ->  8
      BrightnessValue -> 9.37 EV (2267.10 cd/m^2)
      ExposureBiasValue -> 0.00 EV
      MaxApertureValue -> 3.61 EV (f/3.5)
      MeteringMode -> Pattern
      LightSource -> Unknown
      Flash -> Flash did not fire, compulsory flash mode
      FocalLength -> 18.0 mm
      MakerNote -> 4890 bytes undefined data
      UserComment -> 64 bytes undefined data
      FlashPixVersion -> FlashPix Version 1.0
      ColorSpace -> sRGB
      PixelXDimension -> 3872
      PixelYDimension -> 2592
      FileSource -> DSC
      SceneType -> Directly photographed
      CustomRendered -> Normal process
      ExposureMode -> Auto exposure
      WhiteBalance -> Auto white balance
      FocalLengthIn35mmFilm -> 27
      SceneCaptureType -> Landscape
      Contrast -> Normal
      Saturation -> Normal
      Sharpness -> Normal
      ImageUniqueID -> 377d19784e8ea1d75c0554aa09d7dad5

    GPS IFD
      GPSVersionID -> 2.0.0.0
      GPSLatitudeRef -> N
      GPSLatitude -> 46, 28, 0.2900
      GPSLongitudeRef -> E
      GPSLongitude ->  8,  9, 59.9800
      GPSAltitudeRef -> Sea level
      GPSAltitude -> 2793.9912
      GPSTimeStamp -> 11:08:57.00
      GPSMapDatum -> WGS-84
      GPSDateStamp -> 2012:08:10

    Interop IFD
      InteroperabilityIndex -> R98
      InteroperabilityVersion -> 0100
      RelatedImageWidth -> 3872
      RelatedImageLength -> 2592

    1 st IFD (Thumbnail)
      Compression -> JPEG compression
      XResolution -> 72
      YResolution -> 72
      ResolutionUnit -> Inch
      JPEGInterchangeFormat -> 6252
      JPEGInterchangeFormatLength -> 5922


### Similar libraries

The [hsexif](http://hackage.haskell.org/package/hsexif) is a similar library. If in doubt, use *hsexif*!

#### Pros of hexif

* *hexif* can show you all tags in an JPEG file including tags unknown to the program. *hsexif* has also a function called *allExifTags*, however, this prints just a predefined list of tags.

#### Pros of hsexif

* *hsexif* is on Hackage
* *hsexif* has more typed functions to access the Exif values
* *hsexif* supports more files created by different camera brands





