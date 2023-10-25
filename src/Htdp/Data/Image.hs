module Htdp.Data.Image
  ( Image
  , width
  , height
  , rotate

  , Color
  , makeColor
  , makeColorI
  -- , rgbaOfColor
  , mixColors
  , addColors
  -- , dim
  -- , bright
  -- , light
  -- , dark
  -- , withRed
  -- , withGreen
  -- , withBlue
  -- , withAlpha
  -- , greyN

  , black, white, red, green, blue, yellow, cyan, magenta, violet, azure
  , aquamarine, chartreuse, orange
  ) where

import Diagrams.Core ( QDiagram )
import Data.Monoid ( Any )
import Diagrams.Backend.Rasterific
import Diagrams.TwoD ( V2 )
import qualified Data.Colour as C
import qualified Diagrams as D
import Diagrams ( (#), (@@) )
import qualified Data.Colour.SRGB as C
import qualified Data.Colour.Names as C


type Image = QDiagram Rasterific V2 Double Any

width :: Image -> Double
width = D.width

height :: Image -> Double
height = D.height

rotate :: Double -> Image -> Image
rotate degCounterClockwise image = image # D.rotate (degCounterClockwise @@ D.deg)

type Color = C.AlphaColour Double

makeColor :: Double -> Double -> Double -> Double -> Color
makeColor r g b alpha = C.sRGB r g b `C.withOpacity` alpha

makeColorI :: Int -> Int -> Int -> Int -> Color
makeColorI r g b a = color `C.withOpacity` ((fromIntegral a :: Double) / 255)
  where
    color = C.sRGB24 (fromInteger $ toInteger r) (fromInteger $ toInteger g) (fromInteger $ toInteger b)

-- rgbaOfColor :: Color -> (Double, Double, Double, Double)
-- rgbaOfColor color = (r, g, b, C.alphaChannel color)
--   where
--     C.RGB r g b = C.toSRGB $ pureColour color

mixColors :: Double -> Double -> Color -> Color -> Color
mixColors proportionFirst proportionSecond =
  C.blend (proportionFirst / (proportionFirst + proportionSecond))

addColors :: Color -> Color -> Color
addColors = C.over

-- dim :: Color -> Color
-- dim = _

-- bright :: Color -> Color
-- bright = _

-- light :: Color -> Color
-- light = _

-- dark :: Color -> Color
-- dark = _

-- withRed :: Double -> Color -> Color
-- withRed = _

-- withGreen :: Double -> Color -> Color
-- withGreen = _

-- withBlue :: Double -> Color -> Color
-- withBlue = _

-- withAlpha :: Double -> Color -> Color
-- withAlpha = _

-- greyN :: Double -> Color
-- greyN = _

black, white, red, green, blue, yellow, cyan, magenta, violet, azure, aquamarine, chartreuse, orange :: Color
black = C.opaque C.black
white = C.opaque C.white
red = C.opaque C.red
green = C.opaque C.green
blue = C.opaque C.blue
yellow = C.opaque C.yellow
cyan = C.opaque C.cyan
magenta = C.opaque C.magenta
violet = C.opaque C.violet
azure = C.opaque C.azure
aquamarine = C.opaque C.aquamarine
chartreuse = C.opaque C.chartreuse
orange = C.opaque C.orange
