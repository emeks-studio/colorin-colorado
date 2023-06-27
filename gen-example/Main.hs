{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | More docs at https://github.com/diagrams/svg-builder
module Main where

import qualified Data.Foldable as F
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Graphics.Svg
import System.Environment (getArgs)

svg :: Element -> Element
svg content =
  doctype
    <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "300", Height_ <<- "300"]

zero :: Float
zero = 0

one :: Float
one = 1

-- TODO: Calculate center according to file size
centerX :: Float
centerX = 150

centerY :: Float
centerY = 150

radius :: Float
radius = 10

degreesIterator :: Float
degreesIterator = pi / 6 -- 30. degrees

-- TODO: Use to iterate more than 1 ring
ringLevel :: Float
ringLevel = 1

-- | FIXME: For the bottom half of the circle, we need to swap things! (for now is a rainbow)
regions :: M.Map Int T.Text
regions =
  M.fromList
    [ (0, "red"),
      (1, "blue"),
      (2, "yellow"),
      (3, "cyan"),
      (4, "purple"),
      (5, "indigo") --, -- til here goes ok
      -- (6, "violet")
      -- (7, "magenta"),
      -- (8, "orange"),
      -- (9, "brown"),
      -- (10, "green"),
      -- (11, "grey")
    ]

-- | 360 degress/ 30 degrees = 12 regions
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
angleByRegion 11 = 2 * pi
angleByRegion _ = 0

-- we should iterate over degress!
slice :: Int -> Element
slice iterator =
  ( path_
      [ D_ <<- mA p1x p2y -- center
          <> lA p2x p2y
          <> aA rA1 rA1 zero (0 :: Int) (0 :: Int) p3x p3y
          <> lA p4x p4y
          <> aA rA2 rA2 zero (0 :: Int) (1 :: Int) p1x p1y,
        Stroke_ <<- "white",
        Fill_ <<- regions ! (iterator)
      ]
  )
  where
    p1x = 150 + ringLevel * radius * cos ((ringLevel - 1) * (angleByRegion iterator))
    p1y = 150 + ringLevel * radius * sin ((ringLevel - 1) * (angleByRegion iterator))
    p2x = 150 + (1 + ringLevel) * radius * cos ((ringLevel - 1) * (angleByRegion iterator))
    p2y = 150 + (1 + ringLevel) * radius * sin ((ringLevel - 1) * (angleByRegion iterator))
    rA1 = (1 + ringLevel) * radius
    p3x = 150 + (1 + ringLevel) * radius * cos (ringLevel * (angleByRegion iterator))
    p3y = 150 - (1 + ringLevel) * radius * sin (ringLevel * (angleByRegion iterator))
    p4x = 150 + ringLevel * radius * cos (ringLevel * (angleByRegion iterator))
    p4y = 150 - ringLevel * radius * sin (ringLevel * (angleByRegion iterator))
    rA2 = ringLevel * radius

baseShape :: Element
baseShape = circle_ [Cx_ <<- "150", Cy_ <<- "150", R_ <<- (T.pack $ show radius), Stroke_ <<- "black", Fill_ <<- "black"]

contents :: Element
contents = F.foldr (\iterator accShape -> accShape <> slice iterator) baseShape [0 .. (fromIntegral $ M.size regions - 1)]

-- -- | Arc (absolute)
--aA :: RealFloat a =>  a -> a -> a -> a -> a -> a -> a -> Text
--aA rx ry xrot largeFlag sweepFlag x y = T.concat
--  [ "A ", toText rx, ",", toText ry, " ", toText xrot, " ", toText largeFlag
--  , " ", toText sweepFlag, " ", toText x, " ", toText y, " "]

-- | Arc (relative)
-- aR :: RealFloat a =>  a -> a -> a -> a -> a -> a -> a -> Text
-- aR rx ry xrot largeFlag sweepFlag x y =

-- | Example: cabal v2-run gen-example -- example.svg
main :: IO ()
main = do
  [filename] <- getArgs
  let filenameInCurrentDirectoryPath :: FilePath
      filenameInCurrentDirectoryPath = "./" ++ filename
      svgContent :: String
      svgContent = show $ svg contents
  -- Obs: Completly overrides the existing file
  writeFile filenameInCurrentDirectoryPath svgContent
