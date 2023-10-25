module Htdp.Shape
  ( Mode, solid, outline
  , circle
  , ellipse
  , line
  , addLine
  , emptyImage
  , triangle
  , square
  , rectangle
  , rhombus
  ) where

import Htdp.Data.Image ( Image, Color )
import qualified Diagrams.TwoD as D
import qualified Diagrams as D
import Diagrams ( (^&), (@@) )
import Diagrams.Util ( (#) )
import Data.Colour ( transparent )

data Mode = Solid | Outline
  deriving Eq

solid, outline :: Mode
solid = Solid
outline = Outline

applyMode :: Mode -> Color -> Image -> Image
applyMode Solid color image = image # D.fcA color
                                    # D.lcA transparent
applyMode Outline color image = image # D.lcA color

circle :: Double -> Mode -> Color -> Image
circle r mode color = D.circle r # applyMode mode color

ellipse :: Double -> Double -> Mode -> Color -> Image
ellipse w h mode color = D.ellipseXY (w / 2) (h / 2) # applyMode mode color

line :: Double -> Double -> Color -> Image
line x y color = D.strokeLine (D.fromVertices [0 ^& 0, x ^& (-y)]) # D.lcA color

addLine :: Image -> Double -> Double -> Double -> Double -> Color -> Image
addLine image x0 y0 x1 y1 color =
  (D.strokeLocLine (D.fromVertices [x0 ^& (-y0), x1 ^& (-y1)]) # D.lcA color)
  `D.atop`
  (image # D.alignL # D.alignT)

emptyImage :: Image
emptyImage = mempty

triangle :: Double -> Mode -> Color -> Image
triangle sideLength mode color = D.triangle sideLength # applyMode mode color

-- rightTriangle :: Double -> Double -> Mode -> Color -> Image
-- rightTriangle = _

-- isoscelesTriangle :: Double -> Double -> Mode -> Color -> Image
-- isoscelesTriangle = _

-- triangleSSS :: Double -> Double -> Double -> Mode -> Color -> Image
-- triangleSSS = _

-- triangleASS :: Double -> Double -> Double -> Mode -> Color -> Image
-- triangleASS = _

-- triangleSAS :: Double -> Double -> Double -> Mode -> Color -> Image
-- triangleSAS = _

-- triangleSSA :: Double -> Double -> Double -> Mode -> Color -> Image
-- triangleSSA = _

-- triangleAAS :: Double -> Double -> Double -> Mode -> Color -> Image
-- triangleAAS = _

-- triangleASA :: Double -> Double -> Double -> Mode -> Color -> Image
-- triangleASA = _

-- triangleSAA :: Double -> Double -> Double -> Mode -> Color -> Image
-- triangleSAA = _

square :: Double -> Mode -> Color -> Image
square size mode color = D.square size # applyMode mode color

rectangle :: Double -> Double -> Mode -> Color -> Image
rectangle w h mode color = D.rect w h # applyMode mode color

rhombus :: Double -> Double -> Mode -> Color -> Image
rhombus sideLength angle mode color = D.polygon polyOpts # applyMode mode color
  where
    verticalRadius = sideLength * D.cosA ((angle / 2) @@ D.deg)
    horizontalRadius = sideLength * D.sinA ((angle / 2) @@ D.deg)
    polyOpts = D.PolygonOpts
      (D.PolyPolar (replicate 3 $ 90 @@ D.deg) [horizontalRadius, verticalRadius, horizontalRadius, verticalRadius])
      D.NoOrient
      (D.P $ D.V2 0 0)

-- star :: Double -> Mode -> Color -> Image
-- star = _
