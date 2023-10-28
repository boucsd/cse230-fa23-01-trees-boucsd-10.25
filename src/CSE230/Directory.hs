module CSE230.Directory where

import CSE230.Doc
import CSE230.List hiding (Dir)
import qualified Data.List as L
import System.FilePath      (takeDirectory, takeFileName, (</>))
import System.Directory     (doesFileExist, listDirectory)
import Text.Read (Lexeme(String))
import Diagrams (hcat)

-------------------------------------------------------------------------------
-- | Top-level "main" function
-------------------------------------------------------------------------------
main :: [String] -> IO ()
main ["-ls", p]        = showDirectory (normalize p)
main ["-find", p, sub] = showMatchingFiles (normalize p) sub
main args              = putStrLn (errorMsg args)

normalize :: FilePath -> FilePath
normalize p = takeDirectory (p </> ".")

errorMsg :: [String] -> String
errorMsg args = unlines
  [ "Sorry, don't understand: " ++ unwords args ++ ". Try one of"
  , " *  'htree -ls <path> ' to render the directory of all files under <path>"
  , " *  'htree -find <path> sub' to find all files matching 'sub' under <path>"
  ]

-------------------------------------------------------------------------------
-- | 'showDirectory path' builds and displays the 'Dir' rooted at 'path'
-------------------------------------------------------------------------------
-- >>> showDirectory "src"
-- src
-- ├── CSE230
-- │   ├── Directory.hs
-- │   ├── Doc.hs
-- │   ├── Graphics.hs
-- │   ├── List.hs
-- │   └── Shapes.hs
-- ├── Htdp
-- │   ├── Combinator.hs
-- │   ├── Data
-- │   │   └── Image.hs
-- │   ├── README.md
-- │   └── Shape.hs
-- ├── Htdp.hs
-- └── Main.hs
-- <BLANKLINE>
--

showDirectory :: FilePath -> IO ()
showDirectory path = do
  dir <- build path
  print (dirDoc dir)

-------------------------------------------------------------------------------
-- | 'showMatchingFiles path sub' finds the files matching 'sub' (deep) in the
--   the directory rooted at 'path' and prints them out.
-------------------------------------------------------------------------------
-- >>> showMatchingFiles "src" ".hs"
-- src/CSE230/Directory.hs
-- src/CSE230/Doc.hs
-- src/CSE230/Graphics.hs
-- src/CSE230/List.hs
-- src/CSE230/Shapes.hs
-- src/Htdp/Combinator.hs
-- src/Htdp/Data/Image.hs
-- src/Htdp/Shape.hs
-- src/Htdp.hs
-- src/Main.hs
--

showMatchingFiles :: FilePath -> String -> IO ()
showMatchingFiles path sub = do
  dir   <- build path
  mapM_ putStrLn (findFiles sub dir)


-------------------------------------------------------------------------------
-- | The 'Directory' data type
-------------------------------------------------------------------------------
data Dir a
    = Fil a             -- ^ A single file named `a`
    | Sub a [Dir a]     -- ^ A sub-directory name `a` with contents `[Dir a]`
    deriving (Eq, Show)

example :: Dir FilePath
example = Sub "."
            [ Fil "COLLABORATORS.md"
            , Fil "LICENSE"
            , Fil "Makefile"
            , Fil "README.md"
            , Fil "cse230-tree.cabal"
            , Sub "out" [ Fil "carpet.png"
                        , Fil "chess1.png"
                        , Fil "chess2.png"
                        , Fil "rainbow.png"
                        , Fil "triangle1.png"
                        , Fil "triangle2.png"
                        ]
            , Sub "src" [ Sub "CSE230" [ Fil "Directory.hs"
                                       , Fil "Doc.hs"
                                       , Fil "Graphics.hs"
                                       , Fil "List.hs"
                                       , Fil "Shapes.hs"
                                       ]
                        , Sub "Htdp" [ Fil "Combinator.hs"
                                     , Sub "Data" [ Fil "Image.hs" ]
                                     , Fil "README.md"
                                     , Fil "Shape.hs"
                                     ]
                        , Fil "Htdp.hs"
                        , Fil "Main.hs"
                        ]
            , Fil "stack.yaml"
            ]

srcDir :: Dir FilePath
srcDir = Sub "src"
         [ Sub "CSE230" [ Fil "Directory.hs"
                        , Fil "Doc.hs"
                        , Fil "Graphics.hs"
                        , Fil "List.hs"
                        , Fil "Shapes.hs"
                        ]
         , Sub "Htdp" [ Fil "Combinator.hs"
                      , Sub "Data" [ Fil "Image.hs" ]
                      , Fil "README.md"
                      , Fil "Shape.hs"
                      ]
         , Fil "Htdp.hs"
         , Fil "Main.hs"
         ]


-------------------------------------------------------------------------------
-- | Printing Directories
-------------------------------------------------------------------------------

-- >>> dirDoc example
-- .
-- ├── COLLABORATORS.md
-- ├── LICENSE
-- ├── Makefile
-- ├── README.md
-- ├── cse230-tree.cabal
-- ├── out
-- │   ├── carpet.png
-- │   ├── chess1.png
-- │   ├── chess2.png
-- │   ├── rainbow.png
-- │   ├── triangle1.png
-- │   └── triangle2.png
-- ├── src
-- │   ├── CSE230
-- │   │   ├── Directory.hs
-- │   │   ├── Doc.hs
-- │   │   ├── Graphics.hs
-- │   │   ├── List.hs
-- │   │   └── Shapes.hs
-- │   ├── Htdp
-- │   │   ├── Combinator.hs
-- │   │   ├── Data
-- │   │   │   └── Image.hs
-- │   │   ├── README.md
-- │   │   └── Shape.hs
-- │   ├── Htdp.hs
-- │   └── Main.hs
-- └── stack.yaml
-- <BLANKLINE>
--

dirDoc :: Dir FilePath -> Doc 
dirDoc (Fil f)    = doc f  --error "fill this in"
dirDoc (Sub f ds) = vcatL (dirDoc (Fil f)) (subdirDoc ds)--(foldl vcatL empty (map subdirDoc ds))-- $ (hcatB (foldr vcatL empty (map (\x -> dirLinesPad x (length ds)) (zip [1..] ds))) (foldr vcatL empty  (map dirDoc ds))) -- "fill this in"

--dirLines :: Dir Int -> FilePath -> Doc 
--dirLines depth d= hcatT (dirLinesPad 3 True) (dirDoc d)--dash `hcatT` -- length (dirDoc d)

subdirDoc :: [Dir FilePath] -> Doc
subdirDoc ds = hcatB (foldl vcatL empty (map (\x -> dirLinesPad x (length ds)) (zip [1..] ds))) (foldl vcatL empty (map dirDoc ds)) -- dirLinesPad (zip [1..] ds)  (length ds) 

dirLinesPad :: (Int, Dir FilePath) -> Int -> Doc
dirLinesPad (idx, (Fil _)) len = isLast (idx == len)
dirLinesPad (idx, (Sub _ ds)) len = 
  let 
    innerHeight = foldr ((+) . height .dirDoc) 0 ds
    padPrefix = if idx == len then doc " " else bar
    paddedDoc = foldr vcatL empty (replicate innerHeight padPrefix)
  in 
    vcatL (isLast (idx == len)) paddedDoc

isLast :: Bool -> Doc
isLast f
  | f  = angle `hcatT` dash
  | otherwise = stile `hcatT` dash

dirDocScope :: Int -> Doc
dirDocScope h
  |h <= 1 = vcatL angle empty
  |otherwise = vcatL bar (dirDocScope (h - 1))

-------------------------------------------------------------------------------
-- | Some useful 'Doc's--------------------------------------------------------
-------------------------------------------------------------------------------
dash :: Doc
dash = doc "── "

stile :: Doc
stile = doc "├"

angle :: Doc
angle = doc "└"

bar :: Doc
bar = doc "│"

-------------------------------------------------------------------------------
-- | A 'fold' for directories
-------------------------------------------------------------------------------
data DirElem a = SubDir a | File a

foldDir :: ([a] -> r -> DirElem a -> r) -> r -> Dir a -> r
foldDir f = go []
  where
      go stk r (Fil a)    = f stk r (File a)
      go stk r (Sub a ds) = L.foldl' (go stk') r' ds
        where
            r'   = f stk r (SubDir a)
            stk' = a:stk

-------------------------------------------------------------------------------
-- | 'allFiles dir' returns a list of all the Files in 'dir'
-------------------------------------------------------------------------------
-- >>> allFiles example
-- ["COLLABORATORS.md","LICENSE","Makefile","README.md","cse230-tree.cabal","carpet.png","chess1.png","chess2.png","rainbow.png","triangle1.png","triangle2.png","Directory.hs","Doc.hs","Graphics.hs","List.hs","Shapes.hs","Combinator.hs","Image.hs","README.md","Shape.hs","Htdp.hs","Main.hs""stack.yaml"]
--

allFiles :: Dir FilePath -> [FilePath]
allFiles dir = reverse (foldDir f [] dir)
  where 
      f      = go--error "fill this in"
        where
          go stk r (File a) = a:r
          go stk r (SubDir ds) = r

-------------------------------------------------------------------------------
-- | 'allDirs dir' returns a list of all the sub-directories in 'dir'
-------------------------------------------------------------------------------
--
-- >>> allDirs example
-- [".","out","src","CSE230","Htdp","Data"]

allDirs :: Dir FilePath -> [FilePath]
allDirs dir = reverse (foldDir f [] dir)
  where
    f =  go --error "fill this in"
      where
        go stk r (File ds) = r
        go stk r (SubDir a) = a:r

-------------------------------------------------------------------------------
-- | 'findFiles sub dir' returns a list of all the Files-with-paths in 'dir'
--   such that 'sub' is a substring of the Files' names.
-------------------------------------------------------------------------------
--
-- >>> findFiles ".hs" example
-- ["./src/CSE230/Directory.hs","./src/CSE230/Doc.hs","./src/CSE230/Graphics.hs","./src/CSE230/List.hs","./src/CSE230/Shapes.hs","./src/Htdp/Combinator.hs","./src/Htdp/Data/Image.hs","./src/Htdp/Shape.hs","./src/Htdp.hs","./src/Main.hs"]
--

findFiles :: String -> Dir FilePath -> [FilePath]
findFiles sub dir = reverse (foldDir f [] dir)
   where
      f = go-- error "fill this in"
        where
          go stk r (File a)
            |isSubSequence sub a = foldl (\x1 x2 -> x2 </> x1) a stk : r
            |otherwise = r
          go stk r (SubDir a) = r

-------------------------------------------------------------------------------
-- | 'build path' constructing the Directory on the filesystem rooted at 'path'
-------------------------------------------------------------------------------
--
-- >>> build "src"
-- Sub "src" [Sub "CSE230" [Fil "Directory.hs",Fil "Doc.hs",Fil "Graphics.hs",Fil "List.hs",Fil "Shapes.hs"],Sub "Htdp" [Fil "Combinator.hs",Sub "Data" [Fil "Image.hs"],Fil "README.md",Fil "Shape.hs"],Fil "Htdp.hs",Fil "Main.hs"]
--

build :: FilePath -> IO (Dir FilePath)
build = go []-- error "fill this in"
  where
    go :: [FilePath] -> FilePath -> IO (Dir FilePath)
    go stk ds =
      let fullPath = foldr (</>) ds stk in do
        isFile <- doesFileExist fullPath
        if isFile then return (Fil (takeFileName ds))
        else do
          ls <- listDirectory fullPath
          subls <- mapM (go (stk ++ [ds])) (L.sort ls)
          return (Sub ds subls)


lshow :: Doc -> [String]
lshow = lines . show
