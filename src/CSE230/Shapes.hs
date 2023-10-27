module CSE230.Shapes where

import CSE230.List
import CSE230.Graphics
import Htdp

-------------------------------------------------------------------------------
main :: IO ()
-------------------------------------------------------------------------------
main = do
  mkRainbow
  mkChess1
  mkChess2
  mkTriangle1
  mkTriangle2
  mkCarpet

-------------------------------------------------------------------------------
-- | Rainbow with 'map'
-------------------------------------------------------------------------------
mkRainbow :: IO ()
mkRainbow = save "img/rainbow.png" rainbow

rainbow :: Image
rainbow = foldr1 f xs
  where
    xs  = map g [1..7]
    f   = overlay   --overlay
    g x = circle (100 * x) solid (select_color x)
      where 
        select_color y
          | y == 1  = violet
          | y == 2  = blue
          | y == 3  = cyan
          | y == 4  =  green
          | y == 5  =  yellow
          | y == 6  =  orange
          | otherwise = red
      -- | x == 1  = circle 100 solid violet --draw circle with color
      -- | x == 2  = circle 200 solid blue
      -- | x == 3  = circle 300 solid cyan
      -- | x == 4  = circle 400 solid green
      -- | x == 5  = circle 500 solid yellow
      -- | x == 6  = circle 600 solid orange
      -- | otherwise = circle 700 solid red
-------------------------------------------------------------------------------
-- | ChessBoard with 'clone'
-------------------------------------------------------------------------------
mkChess1 :: IO ()
mkChess1   = save "img/chess1.png"   chessBoard1

chessBoard1 :: Image
chessBoard1 = aboves (clone 4 row)
  where
    row     = besides (clone 4 gridSquare)

gridSquare :: Image
gridSquare = aboves [ besides [ whSq, blSq ]
                    , besides [ blSq, whSq ] ]
  where
    whSq   = square 50 solid bgCol
    blSq   = square 50 solid fgCol

-------------------------------------------------------------------------------
-- | ChessBoard with `iter`
-------------------------------------------------------------------------------
mkChess2 :: IO ()
mkChess2   = save "img/chess2.png"   chessBoard2

chessBoard2 :: Image
chessBoard2 = iter 2 f base
  where
    f x     = aboves (clone 2 (besides (clone 2 x))) --error "fill this in"
    base    = gridSquare


-------------------------------------------------------------------------------
-- | Sierpinski Triangle with recursion
-------------------------------------------------------------------------------
mkTriangle1 :: IO ()
mkTriangle1 = save "img/triangle1.png" sierpinskiTriangle1

sierpinskiTriangle1 :: Image
sierpinskiTriangle1 = triRec 8

triRec :: Int -> Image
triRec 0 = blueTriangle
triRec n = aboves [triRec (n - 1), besides [triRec (n - 1), triRec (n - 1)]] -- error "fill this in"

blueTriangle :: Image
blueTriangle = triangle 5 solid fgCol

-------------------------------------------------------------------------------
-- | Sierpinski Triangle with `iter`
-------------------------------------------------------------------------------
mkTriangle2 :: IO ()
mkTriangle2 = save "img/triangle2.png" sierpinskiTriangle2

sierpinskiTriangle2 :: Image
sierpinskiTriangle2 = iter 8 f base
 where
   f x             = aboves [x, besides [x, x]] -- error "fill this in"
   base            = blueTriangle


-------------------------------------------------------------------------------
-- | Sierpinski Carpet with `iter`
-------------------------------------------------------------------------------
mkCarpet :: IO ()
mkCarpet   = save "img/carpet.png" sierpinskiCarpet

sierpinskiCarpet :: Image
sierpinskiCarpet = iter 4 f base
  where
    f  x         = aboves [carpetEdge x, carpetMiddle x, carpetEdge x] 
     where
      carpetEdge m = besides [m, whiteRow m, m, whiteRow m, m]
        where
          whiteRow y  = rectangle (1 * width y / 8) (width y) solid white
      carpetMiddle n = besides [n, whiteSquare n, n]
        where
          whiteSquare y = square (5 * width y / 4) solid white
    base         = blueSquare



blueSquare :: Image
blueSquare =  square 4 solid fgCol
