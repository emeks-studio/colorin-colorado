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

groupByWithIndex :: Int -> [a] -> [(Int, [a])]
groupByWithIndex chunkOf = go chunkOf 1 -- use 1 if ring level starts in 1
  where
    go _ index [] = [(index, [])]
    go n index l
      | n > 0 = (index, take n l) : go n (index + 1) (drop n l)
      | otherwise = error "Negative or zero n"

instance (SvgGenerator ElipticalSvgEncoder) where
  genSvgFromColors _ colors =
    let totalLength = List.length colors
        numberOfRings = calculateNumberOfRings totalLength
        minRadius :: Int
        minRadius = 5
        totalRadius = minRadius * numberOfRings + minRadius -- if ring level in 1
        -- totalRadius = minRadius * numberOfRings
        totalDiameter = 2 * totalRadius
        totalWidth = totalDiameter
        totalHeight = totalDiameter
        center = (fromIntegral totalRadius, fromIntegral totalRadius)
        colorsGroupedByRing = groupByWithIndex slicesPerRing colors
        content =
          F.foldr
            ( \(ringLevel, subColors) acc ->
                F.foldr
                  ( \(iterator, color) acc' ->
                      acc' <> mkSlice center iterator (fromIntegral minRadius) (fromIntegral ringLevel) color
                  )
                  acc
                  (zip [0 .. slicesPerRing - 1] subColors)
            )
            mempty
            colorsGroupedByRing
     in mkSvg totalWidth totalHeight content
