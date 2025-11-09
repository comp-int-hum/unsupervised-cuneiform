module Main (main) where

import Prelude hiding (readFile)
import Data.Either (fromRight)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text (Text)
import Data.ByteString.Lazy (readFile, toStrict)
import qualified Codec.Compression.GZip as GZip
import Test.Hspec (hspec, describe, it, shouldBe, before)
import Paths_unsupervised_cuneiform (getDataFileName)
import qualified UnsupervisedCuneiform as UC

withTextFrom :: String -> IO Text
withTextFrom f = do
  fn <- getDataFileName f
  bs <- readFile fn
  return $ (decodeUtf8Lenient . toStrict . GZip.decompress) bs

withFileName :: String -> IO String
withFileName s = getDataFileName s

main :: IO ()
main = do
  hspec $ do
    describe "Data" $ do
      before (withTextFrom "cdli_trans_sample.atf.gz") $ do
        it "parses ATF" $ \c -> do
          let xs = UC.parseATF c
          (print . last . fromRight []) xs
          (length $ fromRight [] xs) `shouldBe` 7
      before (withTextFrom "cdli_fields_sample.csv.gz") $ do
        it "parses CSV" $ \c -> do
          let xs = UC.parseCSV c
          (print . last . fromRight []) xs
          (length $ fromRight [] xs) `shouldBe` 19
      before (withFileName "P101175_photo.jpg") $ do
        it "parses photo image" $ \c -> do
          xs <- UC.parseImage c
          (print . last . fromRight []) xs
          (length $ fromRight [] xs) `shouldBe` 1
      before (withFileName "P101175_line.jpg") $ do
        it "parses line image" $ \c -> do        
          xs <- UC.parseImage c
          (print . last . fromRight []) xs
          (length $ fromRight [] xs) `shouldBe` 1
      before (withFileName "oracc_sample.zip") $ do
        it "parses ORACC" $ \c -> do          
          xs <- UC.parseORACC c
          (print . last . fromRight []) xs
          (length $ fromRight [] xs) `shouldBe` 1
