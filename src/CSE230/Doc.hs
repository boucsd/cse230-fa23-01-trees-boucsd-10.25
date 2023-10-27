module CSE230.Doc 
  ( 
    -- * A document type 
    Doc 
    
    -- * Constructors
  , empty, doc 

    -- * Accessors
  , width, height

    -- * Example Doc
  , aDoc, bDoc, lineDoc, animals, triangles

    -- * Combinators
  , vcatL, vcatR, hcatB, hcatT

    -- * Properties
  , prop_hcatT_width  
  , prop_hcatT_height 
  , prop_hcatB_width  
  , prop_hcatB_height 
  , prop_vcatL_width  
  , prop_vcatR_width  
  , prop_vcatL_height 
  , prop_vcatR_height 
  
  ) where

import qualified Test.QuickCheck as QC
import           Prelude hiding (maximum)
import CSE230.List ( maximum, pad, Dir (DirL, DirR) )
import Text.ParserCombinators.ReadP (char)

-------------------------------------------------------------------------------
-- | A 'Doc' is a 'String' list
-------------------------------------------------------------------------------
data Doc = D { docLines :: [String] }
  deriving (Eq, Ord)

empty :: Doc
empty = D []

doc :: String -> Doc
doc s = D (lines s)

aDoc :: Doc
aDoc = D [ "a"
         , "aaa"
         , "aaaaa"]

bDoc :: Doc
bDoc = D [ "b"
         , "bbb"] 

lineDoc :: Doc
lineDoc = doc "<----- HERE"

animals :: [Doc]
animals = [ doc "cat"
          , doc "horse"
          , doc "mongoose" 
          ]

-------------------------------------------------------------------------------
-- | Printing a Doc 
-------------------------------------------------------------------------------
-- >>> aDoc
-- a
-- aaa
-- aaaaa
-- <BLANKLINE>
--

instance Show Doc where
  show (D ls) = unlines ls 

-------------------------------------------------------------------------------
-- | Generating Random Docs
-------------------------------------------------------------------------------
instance QC.Arbitrary Doc where
  arbitrary = fmap D QC.arbitrary

-------------------------------------------------------------------------------
-- | Dimensions of a 'Doc'
-------------------------------------------------------------------------------
-- >>> width aDoc
-- 5

width :: Doc -> Int
width (D ls) = maximum 0 (map length ls) 

-- >>> height aDoc
-- 3

height :: Doc -> Int
height (D ls) = length ls

-------------------------------------------------------------------------------
-- | Vertical Concatenation aligned at Left
-------------------------------------------------------------------------------

-- >>> (doc "cat") `vcatL` (doc "horse") `vcatL` (doc "mongoose")
-- cat
-- horse
-- mongoose
--

vcatL :: Doc -> Doc -> Doc
vcatL d1 d2 = D(docLines d1 ++ docLines d2)-- doc(show d1 ++ show d2)  -- error "fill this in"


-------------------------------------------------------------------------------
-- | Vertical Concatenation aligned at Right
-------------------------------------------------------------------------------

-- >>> (doc "cat") `vcatR` (doc "horse") `vcatR` (doc "mongoose")
--      cat
--    horse
-- mongoose
--      
--    
--

vcatR :: Doc -> Doc -> Doc
vcatR d1 d2 = padDocL d1 d2 -- doc(padl (show d1) ++ padl(show d2)) --error "fill this in"

padDocL :: Doc -> Doc -> Doc
padDocL d1 d2= D(map (\x -> replicate (max (width d1) (width d2) - width d1) ' ' ++ x) (docLines d1) ++ map (\x -> replicate (max (width d1) (width d2) - width d2) ' ' ++ x) (docLines d2))

--padDocL d3 = D(map (\x -> replicate (width d3 - length x) ' ' ++ x) (docLines d3))

-- >>>D(map (\x -> replicate (max (width aDoc) (width bDoc) - width bDoc) ' ' ++ x) (docLines aDoc))
--   a
--   aaa
--   aaaaa
-------------------------------------------------------------------------------
-- | Horizontal Concatenation aligned at Top
--   HINT: use `zip` or `zipWith`
-------------------------------------------------------------------------------

-- >>> hcatT aDoc lineDoc
-- a    <----- HERE
-- aaa  
-- aaaaa
-- <BLANKLINE>
--
-- >>> hcatT aDoc bDoc
-- a    b
-- aaa  bbb
-- aaaaa
-- <BLANKLINE>
--
hcatT :: Doc -> Doc -> Doc
hcatT d1 d2 = conbineLines d1 (elongate DirR (height d1) d2)-- error "fill this in"

conbineLines :: Doc -> Doc -> Doc
conbineLines d1 d2 = D (zipWith (++) (docLines (padDocR d1)) (docLines d2))

padDocR :: Doc -> Doc
padDocR d3= D(map (pad DirR (width d3) ' ') (docLines d3))

elongate :: Dir -> Int -> Doc -> Doc
elongate dir h (D ls) = D (pad dir h "" ls) 

-------------------------------------------------------------------------------
-- | Horizontal Concatenation aligned at Bottom
--   HINT: use `zip` or `zipWith`
-------------------------------------------------------------------------------
-- >>> hcatB aDoc lineDoc
-- a    
-- aaa  
-- aaaaa<----- HERE
-- <BLANKLINE>
--
-- >>> hcatB aDoc bDoc
-- a    
-- aaa  b
-- aaaaabbb
-- <BLANKLINE>
--
hcatB :: Doc -> Doc -> Doc
hcatB d1 d2 = conbineLines d1 (elongate DirL (height d1) d2) --error "fill this in"

triangle :: Doc
triangle = D 
  [ "*"
  , "***"
  , "*****" ]

-- >>> foldr vcatL empty triangles
-- *
-- ***
-- *****
-- *    *
-- ***  ***
-- **********
-- *    *    *
-- ***  ***  ***
-- ***************
-- <BLANKLINE>
--

-- >>> foldr vcatR empty triangles
--           *
--           ***
--           *****
--      *    *
--      ***  ***
--      **********
-- *    *    *
-- ***  ***  ***
-- ***************
--
triangles :: [Doc]
triangles = [ triangle
            , triangle `hcatT` triangle
            , triangle `hcatT` triangle `hcatT` triangle ]


-------------------------------------------------------------------------------
-- | Properties of `Doc` combinators ------------------------------------------
-------------------------------------------------------------------------------

prop_hcatT_width  d1 d2 = width  (d1 `hcatT` d2) == width d1 + width d2
prop_hcatT_height d1 d2 = height (d1 `hcatT` d2) == max (height d1) (height d2)
prop_hcatB_width  d1 d2 = width  (d1 `hcatB` d2) == width d1 + width d2
prop_hcatB_height d1 d2 = height (d1 `hcatB` d2) == max (height d1) (height d2)
prop_vcatL_width  d1 d2 = width  (d1 `vcatL` d2) == max (width d1) (width d2)
prop_vcatR_width  d1 d2 = width  (d1 `vcatR` d2) == max (width d1) (width d2)
prop_vcatL_height d1 d2 = height (d1 `vcatL` d2) == height d1 + height d2
prop_vcatR_height d1 d2 = height (d1 `vcatR` d2) == height d1 + height d2
