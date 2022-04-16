module Lib where

import Drawable

data Allele = Z | O deriving (Eq)

instance Show Allele where
  show Z = "0"
  show O = "1"

type Gamete = [Allele]
data Genotype = Genotype { n :: Int, upper :: Gamete, lower :: Gamete } deriving (Show)

parseGamete :: String -> Gamete
parseGamete "" = []
parseGamete ('0':xs) = Z : parseGamete xs
parseGamete ('1':xs) = O : parseGamete xs
parseGamete s = error $ "invalid character in gamete: " ++ s

parseGenotype :: Int -> String  -> String -> Genotype
parseGenotype n s t
  | n == length xu && n == length xl = Genotype n xu xl
  | otherwise = error "mismatched lengths when pasing genotype"
    where
      xu = parseGamete s
      xl = parseGamete t

instance Drawable Genotype where
  getBoundsAndDrawer (Genotype n u l) = (bounds, drawer) where
    bounds = (fromIntegral n, 2)
    drawer (offX, offY) = unlines nodesNfills where
    
      nodesNfills = do
        (i, c) <- zip [0,1] [l,u]
        (j, x) <- zip [0..n-1] c
        let fill = drawfill (offY + i) (offX + fromIntegral j) x
        let node = drawnode (offY + i) (offX + fromIntegral j) x
        [fill, node]

      drawnode i j x = concat
          [ "\\node (X) at ("
          , show $ 0.5 + j
          , ","
          , show $ 0.5 + i
          , ") {"
          , show x
          , "};"
          ]

      drawfill i j x = concat
          [ "\\filldraw["
          , colour x
          , "!40!white, draw=black] "
          , show (j, i)
          , " rectangle "
          , show (j+1, i+1)
          , ";"
          ]


newtype DrawGamete = DrawGamete Gamete
instance Drawable DrawGamete where
  getBoundsAndDrawer (DrawGamete xs) = (bounds, drawer) where
    n = length xs
    bounds = (fromIntegral n, 1)
    drawer (offX, offY) = unlines nodesNfills where
      nodesNfills = do
        (j, x) <- zip [0..n-1] xs
        let fill = drawfill offY (offX + fromIntegral j) x
        let node = drawnode offY (offX + fromIntegral j) x
        [fill, node]

      drawnode i j x = concat
          [ "\\node (X) at ("
          , show $ 0.5 + j
          , ","
          , show $ 0.5 + i
          , ") {"
          , show x
          , "};"
          ]

      drawfill i j x = concat
          [ "\\filldraw["
          , colour x
          , "!40!white, draw=black] "
          , show (j, i)
          , " rectangle "
          , show (j+1, i+1)
          , ";"
          ]


genotype2Grid :: Genotype -> String
genotype2Grid (Genotype n u l) = unlines . concat $ [
    [ drawfill i j x | (i, c) <- zip [0,1] [l,u], (j, x) <- zip [0..n-1] c ],
    [ drawnode i j x | (i, c) <- zip [0,1] [l,u], (j, x) <- zip [0..n-1] c ],
    []] where
      drawgrid n = "\\draw[step=1cm,black,very thin] (0,0) grid (" ++ show n ++ ",2);"

      drawnode i j x = concat
          [ "\\node (X) at ("
          , show j
          , ".5,"
          , show i
          , ".5) {"
          , show x
          , "};"
          ]

      drawfill i j x = concat
          [ "\\filldraw["
          , colour x
          , "!40!white, draw=black] "
          , show (j, i)
          , " rectangle "
          , show (j+1, i+1)
          , ";"
          ]

colour :: Allele -> String
colour Z = "blue"
colour O = "yellow"

genotypeWithCrossover2Grid :: Int -> Genotype -> String
genotypeWithCrossover2Grid k (Genotype n u l) = unlines . concat $ [
    [ drawfill i j x | (i, c) <- zip [0,1] [l,u], (j, x) <- zip [0..n-1] c ],
    [ drawnode i j x | (i, c) <- zip [0,1] [l,u], (j, x) <- zip [0..n-1] c ],
    []] where
      drawgrid n = "\\draw[step=1cm,black,very thin] (0,0) grid (" ++ show n ++ ",2);"

      drawnode i j x = concat
          [ "\\node (X) at ("
          , show j
          , ".5,"
          , show i
          , ".5) {"
          , show x
          , "};"
          ]

      drawfill i j x = concat
          [ "\\filldraw["
          , if even ((n*i + j - k) `div` n) then colour x else "gray"
          , "!40!white, draw=black] "
          , show (j, i)
          , " rectangle "
          , show (j+1, i+1)
          , ";"
          ]

data Arrow = Arrow Float Float
instance Drawable Arrow where
  getBounds (Arrow x y) = (abs x, abs y)
  drawAtOffset (Arrow x y) (offX, offY) = concat 
    [ "\\draw [-to] ("
    , show xi
    , ","
    , show yi
    , ") -- ("
    , show xf
    , ","
    , show yf
    , ");\n"
    ] where
      xi = offX + max 0 (-x)
      xf = offX + max 0 x
      yi = offY + max 0 (-y)
      yf = offY + max 0 y

newtype ArrowUp = ArrowUp Float
newtype ArrowDown = ArrowDown Float
newtype ArrowLeft = ArrowLeft Float
newtype ArrowRight = ArrowRight Float

instance Drawable ArrowUp where
  getBoundsAndDrawer (ArrowUp y) = getBoundsAndDrawer (Arrow 0 y)
instance Drawable ArrowDown where
  getBoundsAndDrawer (ArrowDown y) = getBoundsAndDrawer (Arrow 0 (-y))
instance Drawable ArrowLeft where
  getBoundsAndDrawer (ArrowLeft x) = getBoundsAndDrawer (Arrow x 0)
instance Drawable ArrowRight where
  getBoundsAndDrawer (ArrowRight x) = getBoundsAndDrawer (Arrow (-x) 0)

twistNStitch :: Genotype -> Genotype
twistNStitch = last . twistNStitchWorking

twistNStitchWorking :: Genotype -> [Genotype]
twistNStitchWorking = takeUntil (\g -> upper g == lower g) . iterate tnsStep where
  takeUntil f [] = []
  takeUntil f (x:xs)
    | f x       = x : takeUntil f xs
    | otherwise = [x]

tnsStep :: Genotype -> Genotype
tnsStep (Genotype n xu xl) = Genotype n xu' xl' where
  xl' = xu
  xu' = getNewUpper (zip xu xl)
  getNewUpper [] = []
  getNewUpper ((x, Z):xs) = x : getNewUpper xs
  getNewUpper ((O, O):xs) = O : getNewUpper xs
  getNewUpper ((Z, O):xs) = O : map snd xs

