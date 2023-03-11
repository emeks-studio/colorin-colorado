module ColorinColorado.Svg.SingleLineSvgEncoder (SingleLineSvgGenerator (SingleLineSvgGenerator)) where

import ColorinColorado.Svg.Common (mkRect, mkSvg)
import ColorinColorado.Svg.Encoder (SvgGenerator, genSvgFromColors)
import qualified Data.List as List (foldl', length)

data SingleLineSvgGenerator = SingleLineSvgGenerator

instance (SvgGenerator SingleLineSvgGenerator) where
  genSvgFromColors _ colors =
    let totalLength = List.length colors
        totalWidth = totalLength
        totalHeight = 1
        widthPerElement = 1
        startingXPosition = 0
        fixedYPosition = 0
        (_, content) =
          List.foldl'
            ( \acc color ->
                let (currentXPosition, currentContent) = acc
                    newContent = mkRect currentXPosition fixedYPosition widthPerElement totalHeight color
                 in (currentXPosition + widthPerElement, currentContent <> newContent)
            )
            (startingXPosition, mempty)
            colors
     in mkSvg totalWidth totalHeight content
