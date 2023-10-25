module CSE230.Graphics where

import Htdp
import Diagrams.Backend.Rasterific
import qualified Diagrams.TwoD as D
import Diagrams.Util ( (#) )
import qualified Diagrams.Prelude as D ( white )

-------------------------------------------------------------------------------
-- Save a 'Image' as a PNG file
-------------------------------------------------------------------------------
save :: FilePath -> Image -> IO ()
save f raw = renderRasterific f dim img
  where
    img = raw
          # D.centerXY
          # D.bg D.white
    dim = D.dims2D (scale * width raw) (scale * height raw)
    scale =
      if width raw < 100 && height raw < 100
      then max (100 / width raw) (100 / height raw)
      else 1

{-
save :: FilePath -> Image -> IO ()
save f raw = G.exportPictureToPNG (siz, siz) bgCol f (scale pic)
  where
    pic    = G.Pictures [ G.translate x y p
                          | (p, xy)   <- shapes img
                          , let (x, y) = shift dim xy
                        ]
    siz    = succ . round $ dim
    img    = overlay raw (square dim solid bgCol)
    dim    = max (width raw) (height raw)
-}

-------------------------------------------------------------------------------
-- | Foreground color
-------------------------------------------------------------------------------
fgCol :: Color
fgCol = blue

-------------------------------------------------------------------------------
-- | Background color
-------------------------------------------------------------------------------
bgCol :: Color
bgCol = white

{-
-------------------------------------------------------------------------------
compareBMP :: Int -> FilePath -> FilePath -> IO Bool
-------------------------------------------------------------------------------
compareBMP thresh path1 path2 = do
    bmp1E  <- J.readBitmap path1
    bmp2E  <- J.readBitmap path2
    case (bmp1E, bmp2E) of
      (Left e1, _)         -> fileError path1 e1 >> return False
      (_      , Left e2)   -> fileError path2 e2 >> return False
      (Right b1, Right b2) -> do let pct = cmpImage (J.convertRGB8 b1) (J.convertRGB8 b2)
                                 putStrLn $ "Image difference: " ++ show (path1, path2, pct)
                                 return (pct <= thresh)

fileError :: FilePath -> String -> IO ()
fileError path e = putStrLn $ "Cannot read " ++ path ++ " due to error " ++ e

cmpImage :: J.Image J.PixelRGB8 -> J.Image J.PixelRGB8 -> Int
cmpImage i1 i2
  | w1 == w2 && h1 == h2 = (100 * absDiff) `div` picSize
  | otherwise            = 100
  where
    w1                   = J.imageWidth i1
    w2                   = J.imageWidth i2
    h1                   = J.imageWidth i1
    h2                   = J.imageWidth i2
    absDiff              = (sum [pixelDiff (J.pixelAt i1 x y) (J.pixelAt i2 x y) | x <- [0..w1-1], y <- [0..h1-1]])
    picSize              = (3 * 255 * w1 * h1)

pixelDiff :: J.PixelRGB8 -> J.PixelRGB8 -> Int
pixelDiff (J.PixelRGB8 r1 g1 b1) (J.PixelRGB8 r2 g2 b2) = fromIntegral $ abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2)

readImage :: FilePath -> IO [J.PixelRGB8]
readImage path = do
  bE  <- J.readBitmap path
  case bE of
    Left _  -> error "Oh no"
    Right b -> return [ J.pixelAt i x y | x <- [0..w-1], y <- [0..h-1]]
               where i = J.convertRGB8 b
                     h = J.imageHeight i
                     w = J.imageWidth  i
-}
