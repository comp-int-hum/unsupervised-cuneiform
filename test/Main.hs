module Main (main) where

import Prelude hiding (readFile)
--import Torch.Typed
--import Torch.Typed.Auxiliary
--import GHC.TypeLits
--import Data.Reflection
--import Data.Proxy
--import Control.Monad (foldM)
import Data.Either (fromRight)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text (Text)
import Data.ByteString.Lazy (readFile, toStrict)
import qualified Codec.Compression.GZip as GZip
import Test.Hspec (hspec, describe, it, shouldBe, before)
import Paths_unsupervised_cuneiform (getDataFileName)
import qualified UnsupervisedCuneiform as UC
-- import UnsupervisedCuneiform.MLP (MLPSpec(..), MLP(..))
-- import UnsupervisedCuneiform.CNN (CNNSpec(..), CNN(..))
-- import UnsupervisedCuneiform.RNN (RNNSpec(..), RNN(..))
-- import UnsupervisedCuneiform.AutoEncoder (AutoEncoderSpec(..), AutoEncoder(..))
-- import UnsupervisedCuneiform.GCN (GCNSpec(..), GCN(..))
-- import UnsupervisedCuneiform.StarCoder (StarCoderSpec(..), StarCoder(..))

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


  --reifyNat ((fromIntegral . head) ms) $ trainStarCoder @('Float) @( '( 'CPU, 0 ) )

-- trainMLP :: forall dtype device n . ( KnownNat n
--                                  , KnownDevice device
--                                  , RandDTypeIsValid device dtype
--                                  , KnownDType dtype
--                                  , StandardFloatingPointDTypeValidation device dtype
--                                  , BasicArithmeticDTypeIsValid device dtype
--                                  , ComparisonDTypeIsValid device dtype
--                                  ) => Proxy n -> IO ()
-- trainMLP p = do
--   model <- sample (MLPSpec :: MLPSpec '[ n, 10, n ] dtype device )
--   let initOptim = mkAdam 0 0.9 0.999 (flattenParameters model)
--       learningRate = 0.1
--   (model', opt', _) <- foldM step (model, initOptim, learningRate) [1..100]
--   return ()
--   where
--     step (m, opt, lr) i = do
--       t <- randn :: IO (Tensor device dtype '[20, n])  
--       o <- forwardStoch m t
--       let l = smoothL1Loss @ReduceMean o t
--       print l
--       (m', opt') <- runStep m opt l lr
--       return (m', opt', lr)


-- trainCNN :: forall dtype device n . ( KnownNat n
--                                     , KnownDevice device
--                                     , RandDTypeIsValid device dtype
--                                     , KnownDType dtype
--                                     , StandardFloatingPointDTypeValidation device dtype
--                                     , BasicArithmeticDTypeIsValid device dtype
--                                     , ComparisonDTypeIsValid device dtype
--                                     ) => Proxy n -> IO ()
-- trainCNN p = do
--   let spec = CNNSpec :: CNNSpec '[3, 4, 3] '[ '(5, 3), '(3, 3) ] dtype device
--   model <- sample spec
--   --print model
--   --model <- sample (MLPSpec :: MLPSpec '[ n, 10, n ] dtype device )
--   let initOptim = mkAdam 0 0.9 0.999 (flattenParameters model)
--       learningRate = 0.1
--   (model', opt', _) <- foldM step (model, initOptim, learningRate) [1..100]
--   return ()
--     where
--       step (m, opt, lr) i = do
--         t <- randn :: IO (Tensor device dtype '[20, 3, 7, 7])  
--         o <- forwardStoch m t
--         let l = smoothL1Loss @ReduceMean o t
--         print l
--         (m', opt') <- runStep m opt l lr
--         return (m', opt', lr)


-- trainRNN :: forall dtype device n . ( KnownNat n
--                                     , KnownDevice device
--                                     , RandDTypeIsValid device dtype
--                                     , KnownDType dtype
--                                     , StandardFloatingPointDTypeValidation device dtype
--                                     , BasicArithmeticDTypeIsValid device dtype
--                                     , ComparisonDTypeIsValid device dtype
--                                     ) => Proxy n -> IO ()
-- trainRNN p = do
--   let spec = RNNSpec :: RNNSpec 15 15 dtype device
--   model <- sample spec
--   let initOptim = mkAdam 0 0.9 0.999 (flattenParameters model)
--       learningRate = 0.1
--   (model', opt', _) <- foldM step (model, initOptim, learningRate) [1..100]
--   return ()
--     where
--       step (m, opt, lr) i = do
--         t <- randn :: IO (Tensor device dtype '[20, 10, 15])
--         o <- forwardStoch m t
--         let l = smoothL1Loss @ReduceMean o t
--         print l
--         (m', opt') <- runStep m opt l lr
--         return (m', opt', lr)


-- trainAutoEncoder :: forall dtype device n . ( KnownNat n
--                                             , KnownDevice device
--                                             , RandDTypeIsValid device dtype
--                                             , KnownDType dtype
--                                             , StandardFloatingPointDTypeValidation device dtype
--                                             , BasicArithmeticDTypeIsValid device dtype
--                                             , ComparisonDTypeIsValid device dtype
--                                             ) => Proxy n -> IO ()
-- trainAutoEncoder p = do
--   let spec = AutoEncoderSpec :: AutoEncoderSpec 200 '[100, 50] 25 '[50, 100] dtype device
--   model <- sample spec
--   let initOptim = mkAdam 0 0.9 0.999 (flattenParameters model)
--       learningRate = 0.1
--   return ()
--   (model', opt', _) <- foldM step (model, initOptim, learningRate) [1..100]
--   return ()
--     where
--       step (m, opt, lr) i = do
--         t <- randn :: IO (Tensor device dtype '[20, 200])
--         (emb, recon) <- forwardStoch m t
--         let l = smoothL1Loss @ReduceMean recon t
--         print l
--         (m', opt') <- runStep m opt l lr
--         return (m', opt', lr)


-- trainStarCoder :: forall dtype device n . ( KnownNat n
--                                           , KnownDevice device
--                                           , RandDTypeIsValid device dtype
--                                           , KnownDType dtype
--                                           , StandardFloatingPointDTypeValidation device dtype
--                                           , BasicArithmeticDTypeIsValid device dtype
--                                           , ComparisonDTypeIsValid device dtype
--                                           ) => Proxy n -> IO ()
-- trainStarCoder p = do
--   let spec = StarCoderSpec :: StarCoderSpec Int '[Tablet, Location, Person, Period] dtype device
--   return ()
--   -- let spec = AutoEncoderSpec :: AutoEncoderSpec 200 '[100, 50] 25 '[50, 100] dtype device
--   -- model <- sample spec
--   -- let initOptim = mkAdam 0 0.9 0.999 (flattenParameters model)
--   --     learningRate = 0.1
--   -- return ()
--   -- (model', opt', _) <- foldM step (model, initOptim, learningRate) [1..100]
--   -- return ()
--   --   where
--   --     step (m, opt, lr) i = do
--   --       t <- randn :: IO (Tensor device dtype '[20, 200])
--   --       (emb, recon) <- forwardStoch m t
--   --       let l = smoothL1Loss @ReduceMean recon t
--   --       print l
--   --       (m', opt') <- runStep m opt l lr
--   --       return (m', opt', lr)
