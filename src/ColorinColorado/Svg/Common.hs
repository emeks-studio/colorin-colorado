{-# LANGUAGE OverloadedStrings #-}

-- | About dimensions: Notice that 1 is the minimum "visible" value
module ColorinColorado.Svg.Common
  ( mkRect,
    mkSvg,
  )
where

import qualified ColorinColorado.Types.Colors as Colors (HexColor, toText)
import ColorinColorado.Utils (toText)
import Graphics.Svg
  ( AttrTag (Fill_, Height_, Version_, Width_, X_, Y_),
    Element,
    doctype,
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

mkSvg :: Int -> Int -> Element -> Element
mkSvg width height content =
  doctype
    <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- toText width, Height_ <<- toText height]
