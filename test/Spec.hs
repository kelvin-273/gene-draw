import Test.Hspec
import Drawable
import Data.Void

main :: IO ()
main = hspec $ do
  describe "Basic blocks" $ do

    it "draws tall block" $ do
      drawAtOrigin (Rect 1 2) `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,2.0);\n"

  describe "block placement" $ do
    it "Place item horizontally center aligned" $ do

      drawAtOrigin (HorizontalCenter (Rect 1 2) (Rect 1 1))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,2.0);\n\\filldraw[blue!40!white, draw=black] (1.0,0.5) rectangle (2.0,1.5);\n"

  describe "Attach behaviour" $ do

    it "Attaches left" $ do
      drawAtOrigin (AttachLeftCenter (Rect 1 2) (Rect 1 1))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (-1.0,0.5) rectangle (0.0,1.5);\n\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,2.0);\n"

    it "Attaches right" $ do
      drawAtOrigin (AttachRightCenter (Rect 1 2) (Rect 1 1))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,2.0);\n\\filldraw[blue!40!white, draw=black] (1.0,0.5) rectangle (2.0,1.5);\n"

    it "Attaches above" $ do
      drawAtOrigin (AttachAboveCenter (Rect 2 1) (Rect 1 1))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.5,1.0) rectangle (1.5,2.0);\n\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (2.0,1.0);\n"

    it "Attaches below" $ do
      drawAtOrigin (AttachBelowCenter (Rect 2 1) (Rect 1 1))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (2.0,1.0);\n\\filldraw[blue!40!white, draw=black] (0.5,-1.0) rectangle (1.5,0.0);\n"

  describe "Anchoring the smaller object" $ do

    it "Attaches left" $ do
      drawAtOrigin (AttachLeftCenter (Rect 1 1) (Rect 1 2))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,1.0);\n\\filldraw[blue!40!white, draw=black] (-1.0,-0.5) rectangle (0.0,1.5);\n"

    it "Attaches right" $ do
      drawAtOrigin (AttachRightCenter (Rect 1 1) (Rect 1 2))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,1.0);\n\\filldraw[blue!40!white, draw=black] (1.0,-0.5) rectangle (2.0,1.5);\n"

    it "Attaches above" $ do
      drawAtOrigin (AttachAboveCenter (Rect 1 1) (Rect 1 2))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,1.0);\n\\filldraw[blue!40!white, draw=black] (-0.5,1.0) rectangle (1.5,2.0);\n"

    it "Attaches below" $ do
      drawAtOrigin (AttachBelowCenter (Rect 1 1) (Rect 1 2))
        `shouldBe` "\\filldraw[blue!40!white, draw=black] (0.0,0.0) rectangle (1.0,1.0);\n\\filldraw[blue!40!white, draw=black] (-0.5,-1.0) rectangle (1.5,0.0);\n"

  describe "AboveCenter" $ do
    let x = NodeText "x"
    let y = NodeText "y"
    it "places one above the other" $ do
      drawAtOrigin x `shouldBe` "\\node (X) at (0.5,0.5) {x};\n"
      drawAtOrigin y `shouldBe` "\\node (X) at (0.5,0.5) {y};\n"
      drawAtOrigin (VerticalCenter x y) `shouldBe` "\\node (X) at (0.5,1.5) {x};\n\\node (X) at (0.5,0.5) {y};\n"

data Rect = Rect Float Float
instance Drawable Rect where
  getBounds (Rect a b) = (a, b)
  drawAtOffset (Rect a b) (offX, offY) = concat
    [ "\\filldraw["
    , "blue"
    , "!40!white, draw=black] "
    , show (offX, offY)
    , " rectangle "
    , show (offX+a, offY+b)
    , ";\n"
    ]
