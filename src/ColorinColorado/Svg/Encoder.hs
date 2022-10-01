module ColorinColorado.Svg.Encoder
  ( SvgGenerator,
    genSvgFromColors,
    Encoder,
    encodeFile
  )
where

import ColorinColorado.Types.Painter
  ( coloringFile,
    Painter,
  )
import qualified ColorinColorado.Types.Colors as Colors (HexColor)
import Graphics.Svg
  ( Element,
  )
import Conduit (MonadUnliftIO)

class SvgGenerator a where
  genSvgFromColors :: a -> [Colors.HexColor] -> Element

class Encoder a where
  encodeFile :: (MonadUnliftIO m) => a -> FilePath -> m (Maybe Element)

instance (SvgGenerator g, Painter p) => Encoder (g, p) where
  encodeFile (svgGen, painter) sourceFilePath = do
    mColors <- coloringFile painter sourceFilePath
    let svgGenFn = genSvgFromColors svgGen
    -- In the past we log some info about the gathered colors
    -- TODO: Use a logger with different levels: DEBUG, ERROR, etc
    pure $ svgGenFn <$> mColors
