module ColorinColorado.Svg.ElipticalSvgEncoder (ElipticalSvgEncoder (..)) where

import ColorinColorado.Svg.Common (mkSlice, mkSvg)
import ColorinColorado.Svg.Encoder (SvgGenerator, genSvgFromColors)
import qualified Data.Foldable as F
import qualified Data.List as List (length)

data ElipticalSvgEncoder = ElipticalSvgEncoder

slicesPerRing :: Int
slicesPerRing = 12

calculateNumberOfRings :: Int -> Int
calculateNumberOfRings l =
  l `div` slicesPerRing + if l `rem` slicesPerRing > 0 then 1 else 0

instance (SvgGenerator ElipticalSvgEncoder) where
  genSvgFromColors _ colors =
    let totalLength = List.length colors
        numberOfRings = calculateNumberOfRings totalLength
        minRadius :: Int
        minRadius = 10
        -- totalRadius = minRadius * numberOfRings + minRadius -- if iterator starts in 1
        totalRadius = minRadius * numberOfRings
        totalDiameter = 2 * totalRadius
        totalWidth = totalDiameter
        totalHeight = totalDiameter
        center = (fromIntegral totalRadius, fromIntegral totalRadius)
        -- FIXME: Try to integrate from 12 to 12? (It seems the only difference with original code)
        (_, _, content) =
          F.foldr'
            ( \color acc ->
                let (iterator, ringLevel, currentContent) = acc
                    newContent = mkSlice center iterator (fromIntegral minRadius) ringLevel color
                    updatedIterator = (iterator + 1) `rem` slicesPerRing
                    updatedRingLevel = if updatedIterator == 0 then ringLevel + 1 else ringLevel
                 in (updatedIterator, updatedRingLevel, currentContent <> newContent)
            )
            (0, 0, mempty)
            colors
     in mkSvg totalWidth totalHeight content
