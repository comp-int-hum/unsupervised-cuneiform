{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Torch.Typed
import UnsupervisedCuneiform.TH (makeModel)
import UnsupervisedCuneiform.Domain ( CuneiformDomain
                                      --Tablet(..)
                                    --, Location(..)
                                    --, Person(..)
                                    --, Period(..)
                                    -- , 
                                    )


--makeModel 3 ''CuneiformDomain --[Tablet, Location, Person, Period]


--tempa :: CuneiformDomainModel -> IO ()
--tempa _ = return ()

--tempb :: CuneiformDomainModelSpec 'Float '( 'CPU, 0 ) -> IO ()
--tempb _ = return ()

main :: IO ()
main = do
  --print x
  --tempa undefined
  --tempb undefined
  putStrLn "Hello, Haskell!"

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let
--       dataPath = case args of
--         [] -> error $ "No data path provided"
--         _ -> head args
--   (trainData, testData) <- initMnist dataPath
--   let trainMnist = V.MNIST {batchSize = 32, mnistData = trainData}
--       testMnist = V.MNIST {batchSize = 1, mnistData = testData}
--       spec = MLPSpec 784 64 32 10
--       optimizer = GD
--   init <- sample spec
--   model <- foldLoop init 5 $ \model _ ->
--     runContT (streamFromMap (datasetOpts 2) trainMnist) $ trainLoop model optimizer . fst

--   -- show test images + labels
--   forM_ [0 .. 10] $ displayImages model <=< getItem testMnist

--   putStrLn "Done"
  
