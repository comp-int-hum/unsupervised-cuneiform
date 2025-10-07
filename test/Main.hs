module Main (main) where

import Prelude hiding (readFile)
import Data.Either (fromLeft)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text (Text)
import Data.ByteString.Lazy (readFile, toStrict, ByteString)
import qualified Codec.Compression.GZip as GZip
import Test.Hspec (hspec, describe, it, shouldBe, before)
import Paths_unsupervised_cuneiform (getDataFileName)
import qualified UnsupervisedCuneiform as UC

withTextFrom :: String -> IO Text
withTextFrom f = do
  fn <- getDataFileName f
  bs <- readFile fn
  return $ (decodeUtf8Lenient . toStrict . GZip.decompress) bs

withBytesFrom :: String -> IO ByteString
withBytesFrom f = do
  fn <- getDataFileName f
  readFile fn
  
withFileName :: String -> IO String
withFileName s = getDataFileName s

main :: IO ()
main = do
  hspec $ do
    describe "Data" $ do
      before (withTextFrom "cdli_trans_sample.atf.gz") $ do
        it "parses ATF" $ \c -> do
          (fromLeft "" (UC.parseATF c)) `shouldBe` ""
      before (withTextFrom "cdli_fields_sample.csv.gz") $ do
        it "parses CSV" $ \c -> do
          (fromLeft "" (UC.parseCSV c)) `shouldBe` ""
      before (withBytesFrom "photo_sample.jpg") $ do
        it "parses photo image" $ \c -> do
          (fromLeft "" (UC.parseImage c)) `shouldBe` ""
      before (withBytesFrom "line_sample.jpg") $ do
        it "parses line image" $ \c -> do
          (fromLeft "" (UC.parseImage c)) `shouldBe` ""
      before (withFileName "oracc_sample.zip") $ do
        it "parses ORACC" $ \c -> do
          res <- UC.parseORACC c
          (fromLeft "" res) `shouldBe` ""
