module ColorinColorado.Svg.Encoder
  ( SvgGenerator,
    genSvgFromColors,
    Encoder,
    encodeFile,
  )
where

import qualified ColorinColorado.Types.Colors as Colors (HexColor)
import ColorinColorado.Types.Painter
  ( Painter,
    coloringFile,
  )
import Conduit (MonadUnliftIO)
import Graphics.Svg
  ( Element,
  )

class SvgGenerator a where
  genSvgFromColors :: a -> [Colors.HexColor] -> Element

class Encoder a where
  encodeFile :: (MonadUnliftIO m) => a -> FilePath -> m (Maybe Element)

instance (SvgGenerator g, Painter p) => Encoder (g, p) where
  encodeFile (svgGen, painter) sourceFilePath = do
    mColors <- coloringFile painter sourceFilePath
    -- Debugging!
    -- _ <- liftIO $ print mColors
    let svgGenFn = genSvgFromColors svgGen
    -- In the past we log some info about the gathered colors
    -- TODO: Use a logger with different levels: DEBUG, ERROR, etc
    pure $ svgGenFn <$> mColors
