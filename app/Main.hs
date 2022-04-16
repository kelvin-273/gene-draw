module Main where

import Lib
import Drawable
import System.Hclip

main :: IO ()
main = tns

nonDominatingGenotypes = do
  let x = parseGenotype 4 "0110" "0011"
  let y = parseGenotype 4 "0010" "1010"
  let total = x `HorizontalCenter` SpaceHorizontal 1 `HorizontalCenter` y
  setClipboard . drawAtOrigin $ total

tns = do
  let x  = parseGenotype 8 "10100110" "10010011"
      x1 = parseGenotype 8 "10110011" "10100110"
      x2 = parseGenotype 8 "10110110" "10110011"
      x3 = parseGenotype 8 "10110111" "10110110"
      x4 = parseGenotype 8 "10110111" "10110111"
      ar = ArrowDown 0.5
      --a1 = ar
      --a2 = ar
      --a3 = ar
      --a4 = ar
      a1 = SpaceVertical 0.5 `VerticalCenter` ar `VerticalCenter` SpaceVertical 0.5
      a2 = SpaceVertical 0.5 `VerticalCenter` ar `VerticalCenter` SpaceVertical 0.5
      a3 = SpaceVertical 0.5 `VerticalCenter` ar `VerticalCenter` SpaceVertical 0.5
      a4 = SpaceVertical 0.5 `VerticalCenter` ar `VerticalCenter` SpaceVertical 0.5
      --a1 = AttachRightCenter ar (NodeText "$k = 3$")
      --a2 = AttachRightCenter ar (NodeText "$k = 5$")
      --a3 = AttachRightCenter ar (NodeText "$k = 7$")
      --a4 = AttachRightCenter ar (NodeText "$k = 8$")
      --sep a b = a `VerticalCenter` b
      total = x
        `VerticalCenter` a1 `VerticalCenter` x1
        `VerticalCenter` a2 `VerticalCenter` x2
        `VerticalCenter` a3 `VerticalCenter` x3
        `VerticalCenter` a4 `VerticalCenter` x4

  setClipboard . drawAtOrigin $ total

tnsx = do
  let x' = parseGenotype 8 "01100100" "01100100"
      y' = parseGenotype 8 "11001000" "11001000"
      z  = parseGenotype 8 "01100100" "11001000"
      z' = parseGenotype 8 "11101100" "11101100"
      arrow1 = ArrowLeft 1
      --arrow1 = AttachAboveCenter (ArrowLeft 1) (NodeText "mate")
      arrow2 = ArrowLeft 1
      --arrow2 = AttachAboveCenter (ArrowLeft 1) (NodeText "TnS")
      phase1 = x' `VerticalRight` NodeText "$\\times$" `VerticalRight` y'
      total = phase1
        `HorizontalCenter` SpaceHorizontal 0.5
        `HorizontalCenter` arrow1
        `HorizontalCenter` SpaceHorizontal 0.5
        `HorizontalCenter` z
        `HorizontalCenter` SpaceHorizontal 0.5
        `HorizontalCenter` arrow2
        `HorizontalCenter` SpaceHorizontal 0.5
        `HorizontalCenter` z'
  setClipboard . drawAtOrigin $ total
