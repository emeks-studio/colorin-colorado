{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

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

initialRadius :: Float
initialRadius = 10

-- https://danceswithcode.net/engineeringnotes/rotations_in_2d/rotations_in_2d.html + copilot

-- | Con lo que esta tenemos la pizza, ahora queda:
--  hacer la doble elipse!
--  ~~las otras slides (rotar 30) + loop incremental, ~~ half of the work!
--  calcular centro segun tamanio archivo

-- 12
--

degreesIterator :: Float
degreesIterator = pi / 6 -- 30. degrees

cicle :: Float
cicle = 2 * pi -- 360. degrees

-- [0, 30degreesIterator .. 2pi]

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

slice :: Float -> Float -> Element
slice start iterator =
  path_
    [ D_ <<- mA (150 :: Float) 150 -- center
        <> lA (150 + initialRadius * cos start' :: Float) (150 - initialRadius * sin start')
        <> aA initialRadius initialRadius zero (0 :: Int) (0 :: Int) (150 + initialRadius * cos ((iterator * degreesIterator)) :: Float) (150 - initialRadius * sin ((iterator * degreesIterator)))
        <> z,
      Stroke_ <<- "white",
      Fill_ <<- regions ! (round start)
    ]
  where
    start' = start * degreesIterator

baseShape :: Element
baseShape = circle_ [Cx_ <<- "150", Cy_ <<- "150", R_ <<- (T.pack $ show initialRadius), Stroke_ <<- "black", Fill_ <<- "black"]

contents :: Element
contents = F.foldr (\iterator accShape -> accShape <> slice iterator (iterator + 1)) baseShape [0, 1 .. (fromIntegral $ M.size regions - 1)]

-- -- | Arc (absolute)
--aA :: RealFloat a =>  a -> a -> a -> a -> a -> a -> a -> Text
--aA rx ry xrot largeFlag sweepFlag x y = T.concat
--  [ "A ", toText rx, ",", toText ry, " ", toText xrot, " ", toText largeFlag
--  , " ", toText sweepFlag, " ", toText x, " ", toText y, " "]

-- | Arc (relative)
-- aR :: RealFloat a =>  a -> a -> a -> a -> a -> a -> a -> Text
-- aR rx ry xrot largeFlag sweepFlag x y =

--     rect_   [ Width_ <<- "50", Height_ <<- "50", "#000080" ->> Fill_]
--  <> rect_   [ X_ <<- "50", Width_ <<- "50", Height_ <<- "50", "#800000" ->> Fill_]
--  <> rect_   [ X_ <<- "100", Width_ <<- "50", Height_ <<- "50", "#808000" ->> Fill_]

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
