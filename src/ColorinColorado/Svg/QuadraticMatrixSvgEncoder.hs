module ColorinColorado.Svg.QuadraticMatrixSvgEncoder (QuadraticSvgMatrixGenerator (..)) where

import ColorinColorado.Svg.Common (mkRect, mkSvg)
import ColorinColorado.Svg.Encoder (SvgGenerator, genSvgFromColors)
import qualified Data.List as List (foldl', length)

data QuadraticSvgMatrixGenerator = QuadraticSvgMatrixGenerator

instance (SvgGenerator QuadraticSvgMatrixGenerator) where
  genSvgFromColors _ colors =
    let totalLength = List.length colors
        quadraticFactor = ceiling $ sqrt (fromIntegral totalLength :: Double)
        totalWidth = quadraticFactor
        totalHeight = quadraticFactor
        widthPerElement = 1
        heightPerElement = 1
        startingXPosition = 0
        startingYPosition = 0
        (_, _, content) =
          List.foldl'
            ( \acc color ->
                let (currentXPosition, currentYPosition, currentContent) = acc
                    newContent = mkRect currentXPosition currentYPosition widthPerElement heightPerElement color
                    (updatedXPosition, updatedYPosition) =
                      updatePositions currentXPosition currentYPosition widthPerElement heightPerElement quadraticFactor
                 in (updatedXPosition, updatedYPosition, currentContent <> newContent)
            )
            (startingXPosition, startingYPosition, mempty)
            colors
     in mkSvg totalWidth totalHeight content

updatePositions :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
updatePositions xPos yPos widthPerElement heightPerElement factor =
  if nextXPost < factor
    then (nextXPost, yPos)
    else (0, yPos + heightPerElement)
  where
    nextXPost = xPos + widthPerElement
