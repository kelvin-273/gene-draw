{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Drawable where

import GHC.TypeNats

type Offset = (Float, Float)
type Bounds = Offset
type LaTeX = String

class Drawable a where
  drawAtOrigin        :: a -> LaTeX
  drawAtOrigin = flip drawAtOffset (0, 0)

  getBounds           :: a -> Bounds
  getBounds = fst . getBoundsAndDrawer

  drawAtOffset        :: a -> Offset -> LaTeX
  drawAtOffset = snd . getBoundsAndDrawer

  getBoundsAndDrawer  :: a -> (Bounds, Offset -> LaTeX)
  getBoundsAndDrawer x = (getBounds x, drawAtOffset x)

  {-# MINIMAL getBounds, drawAtOffset | getBoundsAndDrawer #-}

data HorizontalBottom a b = HorizontalBottom a b

instance (Drawable a, Drawable b) => Drawable (HorizontalBottom a b) where
  getBoundsAndDrawer (HorizontalBottom a b) = (newBounds, newDraw) where

    ((ax, ay), fa) = getBoundsAndDrawer a
    ((bx, by), fb) = getBoundsAndDrawer b

    newBounds = (ax + bx, max ay by)
    newDraw (offX, offY) = fa (offX, offY) ++ fb (offX + ax, offY)

data HorizontalTop a b = HorizontalTop a b

instance (Drawable a, Drawable b) => Drawable (HorizontalTop a b) where
  getBoundsAndDrawer (HorizontalTop a b) = (newBounds, newDraw) where

    ((ax, ay), fa) = getBoundsAndDrawer a
    ((bx, by), fb) = getBoundsAndDrawer b

    newBounds = (ax + bx, max ay by)
    newAy = max 0 (by - ay)
    newBy = max 0 (ay - by)
    newDraw (offX, offY) = fa (offX, newAy) ++ fb (offX + ax, newBy)

data HorizontalCenter a b = HorizontalCenter a b

instance (Drawable a, Drawable b) => Drawable (HorizontalCenter a b) where
  getBoundsAndDrawer (HorizontalCenter a b) = (newBounds, newDraw) where

    ((ax, ay), fa) = getBoundsAndDrawer a
    ((bx, by), fb) = getBoundsAndDrawer b

    newBounds = (ax + bx, max ay by)
    newAy = max 0 (by - ay) / 2
    newBy = max 0 (ay - by) / 2
    newDraw (offX, offY) = fa (offX, newAy) ++ fb (offX + ax, newBy)

data VerticalLeft a b = VerticalLeft a b

instance (Drawable a, Drawable b) => Drawable (VerticalLeft a b) where
  getBoundsAndDrawer (VerticalLeft a b) = (newBounds, newDraw) where

    ((ax, ay), fa) = getBoundsAndDrawer a
    ((bx, by), fb) = getBoundsAndDrawer b

    newBounds = (max ax bx, ay + by)
    newDraw (offX, offY) = fa (offX, offY + by) ++ fb (offX, offY)

data VerticalRight a b = VerticalRight a b

instance (Drawable a, Drawable b) => Drawable (VerticalRight a b) where
  getBoundsAndDrawer (VerticalRight a b) = (newBounds, newDraw) where

    ((ax, ay), fa) = getBoundsAndDrawer a
    ((bx, by), fb) = getBoundsAndDrawer b

    newBounds = (max ax bx, ay + by)
    newAx = max 0 (bx - ax)
    newBx = max 0 (ax - bx)
    newDraw (offX, offY) = fa (newAx, offY + by) ++ fb (newBx, offY)

data VerticalCenter a b = VerticalCenter a b

instance (Drawable a, Drawable b) => Drawable (VerticalCenter a b) where
  getBoundsAndDrawer (VerticalCenter a b) = (newBounds, newDraw) where

    ((ax, ay), fa) = getBoundsAndDrawer a
    ((bx, by), fb) = getBoundsAndDrawer b

    newBounds = (max ax bx, ay + by)
    newAx = max 0 ((bx - ax) / 2)
    newBx = max 0 ((ax - bx) / 2)
    newDraw (offX, offY) = fa (newAx, offY + by) ++ fb (newBx, offY)

newtype SpaceHorizontal = SpaceHorizontal Float
instance Drawable SpaceHorizontal where
  getBoundsAndDrawer (SpaceHorizontal n) = ((n, 0), const "")

newtype SpaceVertical = SpaceVertical Float
instance Drawable SpaceVertical where
  getBoundsAndDrawer (SpaceVertical n) = ((0, n), const "")

data Space = Space Float Float
instance Drawable Space where
  getBoundsAndDrawer (Space x y) = ((x, y), const "")

newtype NodeText = NodeText String
instance Drawable NodeText where
  getBoundsAndDrawer (NodeText s) = ((1, 1), \(offX, offY) -> concat
    [ "\\node (X) at ("
    , show $ offX + 0.5
    , ","
    , show $ offY + 0.5
    , ") {"
    , s
    , "};\n"
    ])

drawnode :: (Num n, Show n, Show a) => n -> n -> a -> LaTeX
drawnode i j x = concat
    [ "\\node (X) at ("
    , show j
    , ".5,"
    , show i
    , ".5) {"
    , show x
    , "};"
    ]

data Orientation = Vertical | Horizontal

data AlignmentVertical = VLeft | VCenter | VRight
data AlignmentHorizontal = HLeft | HCenter | HRight

data AttachAboveCenter a b = AttachAboveCenter a b
instance (Drawable a, Drawable b) => Drawable (AttachAboveCenter a b) where
  getBounds (AttachAboveCenter a b) = getBounds a
  drawAtOffset (AttachAboveCenter a b) = drawAtOffset (VerticalCenter b a)

data AttachBelowCenter a b = AttachBelowCenter a b
instance (Drawable a, Drawable b) => Drawable (AttachBelowCenter a b) where
  getBounds (AttachBelowCenter a b) = getBounds a
  drawAtOffset (AttachBelowCenter a b) (offX, offY) =
    drawAtOffset (VerticalCenter a b) (newX, newY) where
      (boundBX, boundBY) = getBounds b
      (newX, newY) = (offX, offY - boundBY)

data AttachLeftCenter a b  = AttachLeftCenter a b
instance (Drawable a, Drawable b) => Drawable (AttachLeftCenter a b) where
  getBounds (AttachLeftCenter a b) = getBounds a
  drawAtOffset (AttachLeftCenter a b) (offX, offY) =
    drawAtOffset (HorizontalCenter b a) (newX, newY) where
      (boundBX, boundBY) = getBounds b
      (newX, newY) = (offX - boundBX, offY)

data AttachRightCenter a b = AttachRightCenter a b
instance (Drawable a, Drawable b) => Drawable (AttachRightCenter a b) where
  getBounds (AttachRightCenter a b) = getBounds a
  drawAtOffset (AttachRightCenter a b) = drawAtOffset (HorizontalCenter a b)

data Vect (n :: Nat) (a :: *) where
  VNull :: Vect 0 a
  --VCons :: a -> Vect n a -> Vect (n+1) a

type family Chain (n :: Nat) (sep :: * -> * -> *) (item :: *) where
  Chain 0 _ item = item
  Chain (_ n) sep item = sep item (Chain n sep item)

