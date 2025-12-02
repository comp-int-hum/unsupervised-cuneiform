{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Torch.Typed
import GHC.TypeLits
import Data.Reflection
import Data.Proxy
import Control.Monad (foldM)
import UnsupervisedCuneiform.TH (makeModel)
import UnsupervisedCuneiform.Config (readConfig, Config(..))
import UnsupervisedCuneiform.MLP (MLPSpec(..), MLP(..))
import UnsupervisedCuneiform.Domain ( CuneiformDomain
                                      --Tablet(..)
                                    --, Location(..)
                                    --, Person(..)
                                    --, Period(..)
                                    -- , 
                                    )


--makeModel ''CuneiformDomain --[Tablet, Location, Person, Period]


--tempa :: CuneiformDomainModel -> IO ()
--tempa _ = return ()

--tempb :: CuneiformDomainModelSpec 'Float '( 'CPU, 0 ) -> IO ()
--tempb _ = return ()

main :: IO ()
main = do
  ps <- readConfig
  let ms = read (mlpSize ps) :: [Int]
  reifyNat ((fromIntegral . head) ms) $ train @('Float) @( '( 'CPU, 0 ) )

train :: forall dtype device n . ( KnownNat n
                                 , KnownDevice device
                                 , RandDTypeIsValid device dtype
                                 , KnownDType dtype
                                 , StandardFloatingPointDTypeValidation device dtype
                                 , BasicArithmeticDTypeIsValid device dtype
                                 , ComparisonDTypeIsValid device dtype
                                 ) => Proxy n -> IO ()
train p = do
  model <- sample (MLPSpec :: MLPSpec '[ n, 10, n ] dtype device )
  let initOptim = mkAdam 0 0.9 0.999 (flattenParameters model)
      learningRate = 0.1
  (model', opt', _) <- foldM step (model, initOptim, learningRate) [1..100]
  return ()
  where
    step (m, opt, lr) i = do
      t <- randn :: IO (Tensor device dtype '[20, n])  
      o <- forwardStoch m t
      let l = smoothL1Loss @ReduceMean o t
      print l
      (m', opt') <- runStep m opt l lr
      return (m', opt', lr)
