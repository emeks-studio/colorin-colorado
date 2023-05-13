module Main (main) where

import qualified ColorinColorado.Types.Colors as Colors
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.Word (Word8)
import qualified Test.Hspec
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property)
import qualified Test.QuickCheck as Quickcheck (forAll, property)
import Test.QuickCheck.Gen (choose)

main :: IO ()
main = Test.Hspec.hspec $ do
  colorsSpec

colorsSpec :: Test.Hspec.Spec
colorsSpec = do
  Test.Hspec.describe "ColorsSpec" $ do
    Test.Hspec.it "3 bytes should generate a valid RGB (HexColor)" $ do
      Quickcheck.property prop_3bytesFormAValidRGAHexColor
    Test.Hspec.it "3 length (at most) ByteStrings should generate a valid RGB (HexColor)" $ do
      Quickcheck.property prop_3lengthAtMostBytestringFormAValidRGBHexColor
    Test.Hspec.it "4 bytes should generate a valid RGBA (HexColor)" $ do
      Quickcheck.property prop_4bytesFormAValidRGBAHexColor
    Test.Hspec.it "4 length (at most) ByteStrings should generate a valid RGBA (HexColor)" $ do
      Quickcheck.property prop_4lengthAtMostBytestringFormAValidRGBAHexColor

prop_3bytesFormAValidRGAHexColor :: (Word8, Word8, Word8) -> IO ()
prop_3bytesFormAValidRGAHexColor bytes = do
  let color = Colors.fromRGB bytes
  let result = Colors.parseHexColorFromText (Colors.toText color)
  result `Test.Hspec.shouldBe` Right color

prop_4bytesFormAValidRGBAHexColor :: (Word8, Word8, Word8, Word8) -> IO ()
prop_4bytesFormAValidRGBAHexColor bytes = do
  let color = Colors.fromRGBA bytes
  let result = Colors.parseHexColorFromText (Colors.toText color)
  result `Test.Hspec.shouldBe` Right color

prop_3lengthAtMostBytestringFormAValidRGBHexColor :: Property
prop_3lengthAtMostBytestringFormAValidRGBHexColor = Quickcheck.forAll (genNonEmptySizedByteStrings 3) $ \bs -> do
  let result = Colors.parseHexColorFromRGBA bs
  isRight result `Test.Hspec.shouldBe` True

prop_4lengthAtMostBytestringFormAValidRGBAHexColor :: Property
prop_4lengthAtMostBytestringFormAValidRGBAHexColor = Quickcheck.forAll (genNonEmptySizedByteStrings 4) $ \bs -> do
  let result = Colors.parseHexColorFromRGBA bs
  isRight result `Test.Hspec.shouldBe` True

genNonEmptySizedByteStrings :: Int -> Gen ByteString
genNonEmptySizedByteStrings size = do
  randomSize <- choose (1, size)
  rawBs <- replicateM randomSize arbitrary :: Gen [Word8]
  return $ BS.pack rawBs
