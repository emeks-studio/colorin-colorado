{-# LANGUAGE OverloadedStrings #-}

-- | About dimensions: Notice that 1 is the minimum "visible" value
module ColorinColorado.Svg.Common
  ( mkRect,
    mkSvg,
    mkSlice,
    baseShape,
  )
where

import qualified ColorinColorado.Types.Colors as Colors (HexColor, toText)
import ColorinColorado.Utils (toText)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Graphics.Svg
  ( AttrTag (Cx_, Cy_, D_, Fill_, Height_, R_, Stroke_, Version_, Width_, X_, Y_),
    Element,
    aA,
    circle_,
    doctype,
    lA,
    mA,
    path_,
    rect_,
    svg11_,
    with,
    (->>),
    (<<-),
  )

-- TODO: Create some data type called dimension or something
mkRect :: Int -> Int -> Int -> Int -> Colors.HexColor -> Element
mkRect xPosition yPosition width height color =
  rect_
    [ X_ <<- toText xPosition,
      Y_ <<- toText yPosition,
      Width_ <<- toText width,
      Height_ <<- toText height,
      Colors.toText color ->> Fill_
    ]

regions :: M.Map Int T.Text
regions =
  M.fromList
    [ (0, "red"),
      (1, "blue"),
      (2, "yellow"),
      (3, "cyan"),
      (4, "purple"),
      (5, "indigo"),
      (6, "violet"),
      (7, "magenta"),
      (8, "orange"),
      (9, "brown"),
      (10, "green"),
      (11, "grey")
    ]

-- | FIXME: Use color passed!
mkSlice ::
  -- | center
  (Float, Float) ->
  -- | iterator
  Int ->
  -- | radius
  Float ->
  -- | ring level
  Float ->
  -- | color
  Colors.HexColor ->
  Element
mkSlice (centerX, centerY) iterator radius ringLevel _color =
  path_
    [ D_ <<- mA p1x p1y -- initial point
        <> lA p2x p2y
        <> aA rA1 rA1 xAxisRotation (largeArcFlagByRegion iterator) (outerSweepFlagByRegion iterator) p3x p3y
        <> lA p4x p4y
        <> aA rA2 rA2 xAxisRotation (largeArcFlagByRegion iterator) (innerSweepFlagByRegion iterator) p1x p1y,
      Stroke_ <<- "white",
      Fill_ <<- regions ! iterator
    ]
  where
    p1x = centerX + ringLevel * radius
    p1y = centerY
    p2x = centerX + (1 + ringLevel) * radius
    p2y = centerY
    rA1 = (1 + ringLevel) * radius
    p3x = centerX + (1 + ringLevel) * radius * cos (angleByRegion iterator)
    p3y = centerY - (1 + ringLevel) * radius * sin (angleByRegion iterator)
    p4x = centerX + ringLevel * radius * cos (angleByRegion iterator)
    p4y = centerY - ringLevel * radius * sin (angleByRegion iterator)
    rA2 = ringLevel * radius
    -- 360 degres (2 * pi) / 30 degrees (pi / 6) = 12 regions
    angleByRegion :: Int -> Float
    angleByRegion 0 = pi / 6
    angleByRegion 1 = pi / 3
    angleByRegion 2 = pi / 2
    angleByRegion 3 = 2 * pi / 3
    angleByRegion 4 = 5 * pi / 6
    angleByRegion 5 = pi
    angleByRegion 6 = 7 * pi / 6
    angleByRegion 7 = 4 * pi / 3
    angleByRegion 8 = 3 * pi / 2
    angleByRegion 9 = 5 * pi / 3
    angleByRegion 10 = 11 * pi / 6
    angleByRegion 11 = pi / 6
    angleByRegion _ = 0
    largeArcFlagByRegion :: Int -> Int
    largeArcFlagByRegion n
      | n <= 5 = 0
      | otherwise = 1 -- large arc
    outerSweepFlagByRegion :: Int -> Int
    outerSweepFlagByRegion n
      | n == 11 = 1 -- clockwise
      | otherwise = 0
    innerSweepFlagByRegion :: Int -> Int
    innerSweepFlagByRegion n
      | n == 11 = 0
      | otherwise = 1 -- clockwise
    xAxisRotation :: Float
    xAxisRotation = 0

baseShape :: (Float, Float) -> Float -> Element
baseShape (centerX, centerY) radius =
  circle_
    [ Cx_ <<- T.pack (show centerX),
      Cy_ <<- T.pack (show centerY),
      R_ <<- T.pack (show radius),
      Stroke_ <<- "black",
      Fill_ <<- "black"
    ]

mkSvg :: Int -> Int -> Element -> Element
mkSvg width height content =
  doctype
    <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- toText width, Height_ <<- toText height]
