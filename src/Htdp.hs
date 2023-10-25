module Htdp
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

  , Mode, solid, outline
  , circle
  , ellipse
  , line
  , addLine
  , emptyImage
  , triangle
  , square
  , rectangle
  , rhombus

  , Alignment, low, mid, high
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

import Htdp.Data.Image
import Htdp.Shape
import Htdp.Combinator
