module Htdp.Combinator
  ( Alignment, low, mid, high
  , overlay
  , overlayAlign
  , overlayOffset
  , overlayAlignOffset
  , overlayXY
  , underlay
  , underlayAlign
  , underlayOffset
  , underlayXY
  , underlayAlignOffset
  , beside
  , besides
  , besideAlign
  , besidesAlign
  , above
  , aboves
  , aboveAlign
  , abovesAlign
  ) where

import qualified Diagrams as D
import Diagrams.Util ( (#) )
import Htdp.Data.Image

data Alignment = Low | Mid | High

low, mid, high :: Alignment
low = Low
mid = Mid
high = High

overlay :: Image -> Image -> Image
overlay = D.atop

overlayAlign :: Alignment -> Alignment -> Image -> Image -> Image
overlayAlign xAlignment yAlignment i1 i2 = overlayAlignOffset xAlignment yAlignment i1 0 0 i2

overlayOffset :: Image -> Double -> Double -> Image -> Image
overlayOffset i1 x y i2 = overlayAlignOffset mid mid i1 x y i2

xAlignFunc :: Alignment -> Image -> Image
xAlignFunc Low = D.alignL
xAlignFunc Mid = D.centerX
xAlignFunc High = D.alignR

yAlignFunc :: Alignment -> Image -> Image
yAlignFunc Low = D.alignB
yAlignFunc Mid = D.centerY
yAlignFunc High = D.alignT

overlayAlignOffset :: Alignment -> Alignment -> Image -> Double -> Double -> Image -> Image
overlayAlignOffset xAlignment yAlignment i1 x y i2 = newImage # D.centerXY
  where
    newImage = i1 # xAlignFunc xAlignment # yAlignFunc yAlignment
              `D.atop`
              i2 # xAlignFunc xAlignment # yAlignFunc yAlignment # D.moveOriginBy (D.V2 (-x) y)

overlayXY :: Image -> Double -> Double -> Image -> Image
overlayXY i1 x y i2 = overlayAlignOffset low high i1 x y i2

underlay :: Image -> Image -> Image
underlay i1 i2 = overlay i2 i1

underlayAlign :: Alignment -> Alignment -> Image -> Image -> Image
underlayAlign xAlignment yAlignment i1 i2 = overlayAlign xAlignment yAlignment i2 i1

underlayOffset :: Image -> Double -> Double -> Image -> Image
underlayOffset i1 x y i2 = overlayOffset i2 (-x) (-y) i1

underlayXY :: Image -> Double -> Double -> Image -> Image
underlayXY i1 x y i2 = underlayAlignOffset low high i1 x y i2

underlayAlignOffset :: Alignment -> Alignment -> Image -> Double -> Double -> Image -> Image
underlayAlignOffset xAlignment yAlignment i1 x y i2 = overlayAlignOffset xAlignment yAlignment i2 (-x) (-y) i1
  -- i2 # xAlignFunc xAlignment # yAlignFunc yAlignment # D.translateX (-x) # D.translateY (-y)
  -- `D.atop`
  -- i1 # xAlignFunc xAlignment # yAlignFunc yAlignment

beside :: Image -> Image -> Image
beside i1 i2 = (i1 D.||| i2) # D.centerXY

besides :: [Image] -> Image
besides is = D.hcat is # D.centerXY

besideAlign :: Alignment -> Image -> Image -> Image
besideAlign yAlignment i1 i2 = beside (i1 # yAlignFunc yAlignment) (i2 # yAlignFunc yAlignment)

besidesAlign :: Alignment -> [Image] -> Image
besidesAlign yAlignment is = besides (yAlignFunc yAlignment <$> is)

above :: Image -> Image -> Image
above i1 i2 = (i1 D.=== i2) # D.centerXY

aboves :: [Image] -> Image
aboves is = D.vcat is # D.centerXY

aboveAlign :: Alignment -> Image -> Image -> Image
aboveAlign xAlignment i1 i2 = above (i1 # xAlignFunc xAlignment) (i2 # xAlignFunc xAlignment)

abovesAlign :: Alignment -> [Image] -> Image
abovesAlign xAlignment is = aboves (xAlignFunc xAlignment <$> is)

-- TODO: figure out how to do proper cropping/placeImage functions

-- placeImage :: Image -> Double -> Double -> Image -> Image
-- placeImage image x y scene = placeImageAlign image x y mid mid scene

-- placeImages :: [Image] -> [(Double, Double)] -> Image -> Image
-- placeImages images coords scene = foldl (\tempScene (i, (x, y)) -> placeImage i x y tempScene) scene (zip images coords)

-- placeImageAlign :: Image -> Double -> Double -> Alignment -> Alignment -> Image -> Image
-- placeImageAlign image x y xAlignment yAlignment scene = newImage # D.snugCenterXY -- # D.clipTo (D.boundingRect scene)
--   where
--     newImage = image # xAlignFunc xAlignment # yAlignFunc yAlignment # D.translateX x # D.translateY (-y)
--                `D.atop`
--                scene # D.alignTL

-- placeImagesAlign :: [Image] -> [(Double, Double)] -> Alignment -> Alignment -> Image -> Image
-- placeImagesAlign images coords xAlignment yAlignment scene = newImage # D.snugCenterXY
--   where
--     toPoint (x, y) = D.P $ D.V2 x (-y)
--     placedImages = zip (toPoint <$> coords) (xAlignFunc xAlignment . yAlignFunc yAlignment <$> images)
--     newImage = D.position $ placedImages <> [(D.P $ D.V2 0 0, scene # D.alignTL)]
