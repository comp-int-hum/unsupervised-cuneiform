
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
{-# LANGUAGE UndecidableInstances #-}

module UnsupervisedCuneiform.MLP ( MLPLayerStack(..)
                                 ) where

import Prelude hiding (tanh)
import Data.Char (toLower, toUpper)
import Data.Proxy
import Foreign.ForeignPtr
import GHC.Generics hiding (NoSourceUnpackedness, NoSourceStrictness)
import GHC.TypeLits
import GHC.TypeLits.Extra
import GHC.Types
import System.Environment
import System.IO.Unsafe
import Torch.Internal.Managed.Type.Context (manual_seed_L)
import Torch.Typed
import Torch.Typed (DType, DeviceType)
import Torch.DType
import Control.Monad


type MLPLayer input output dtype device = Linear input output dtype device


data MLPLayerStackSpec (layerSizes :: [Nat]) (dtype :: DType) (device :: (DeviceType, Nat)) = MLPLayerStackSpec deriving (Show, Eq)


data MLPLayerStack (layerSizes :: [Nat]) (dtype :: DType) (device :: (DeviceType, Nat)) where
  MLPLayerStack1 :: MLPLayer 10 10 dtype device -> MLPLayerStack '[10] dtype device
  MLPLayerStackK :: MLPLayer 10 10 dtype device ->
                    MLPLayerStack layerSizes dtype device ->
                    MLPLayerStack (10 ': layerSizes) dtype device


deriving instance Show (MLPLayerStack layerSizes dtype device)




class MLPLayerStackParameterized (flag :: Bool) inputSize hiddenSize numLayers directionality dtype device where
  type MLPLayerStackParameters flag inputSize hiddenSize numLayers directionality dtype device :: [Type]
  mlpLayerStackFlattenParameters ::
    Proxy flag ->
    MLPLayerStack inputSize hiddenSize numLayers directionality dtype device ->
    HList (MLPLayerStackParameters flag inputSize hiddenSize numLayers directionality dtype device)
  mlpLayerStackReplaceParameters ::
    Proxy flag ->
    MLPLayerStack inputSize hiddenSize numLayers directionality dtype device ->
    HList (MLPLayerStackParameters flag inputSize hiddenSize numLayers directionality dtype device) ->
    MLPLayerStack inputSize hiddenSize numLayers directionality dtype device

-- instance
--   Parameterized (Linear inputSize outputSize dtype device) =>
--   MLPLayerStackParameterized inputSize outputSize 1 dtype device
--   where
--   type
--     MLPLayerStackParameters 'False inputSize hiddenSize 1 directionality dtype device =
--       Parameters (MLPLayer inputSize hiddenSize directionality dtype device)
--   mlpLayerStackFlattenParameters _ (MLPLayer1 mlpLayer) = flattenParameters mlpLayer
--   mlpLayerStackReplaceParameters _ (MLPLayer1 mlpLayer) parameters = MLPLayer1 $ replaceParameters mlpLayer parameters



-- instance
--   ( Parameterized
--       ( LSTMLayer
--           (hiddenSize * NumberOfDirections directionality)
--           hiddenSize
--           directionality
--           dtype
--           device
--       ),
--     Parameterized (LSTMLayerStack inputSize hiddenSize (numLayers - 1) directionality dtype device),
--     HAppendFD
--       (Parameters (LSTMLayerStack inputSize hiddenSize (numLayers - 1) directionality dtype device))
--       (Parameters (LSTMLayer (hiddenSize * NumberOfDirections directionality) hiddenSize directionality dtype device))
--       ( Parameters (LSTMLayerStack inputSize hiddenSize (numLayers - 1) directionality dtype device)
--           ++ Parameters (LSTMLayer (hiddenSize * NumberOfDirections directionality) hiddenSize directionality dtype device)
--       ),
--     1 <= numLayers,
--     numLayersM1 ~ numLayers - 1,
--     0 <= numLayersM1
--   ) =>
--   LSTMLayerStackParameterized 'True inputSize hiddenSize numLayers directionality dtype device
--   where
--   type
--     LSTMLayerStackParameters 'True inputSize hiddenSize numLayers directionality dtype device =
--       Parameters (LSTMLayerStack inputSize hiddenSize (numLayers - 1) directionality dtype device)
--         ++ Parameters (LSTMLayer (hiddenSize * NumberOfDirections directionality) hiddenSize directionality dtype device)
--   lstmLayerStackFlattenParameters _ (LSTMLayerK lstmLayer lstmLayerStack) =
--     let parameters = flattenParameters lstmLayer
--         parameters' = flattenParameters @(LSTMLayerStack inputSize hiddenSize numLayersM1 directionality dtype device) lstmLayerStack
--      in parameters' `happendFD` parameters
--   lstmLayerStackReplaceParameters _ (LSTMLayerK lstmLayer lstmLayerStack) parameters'' =
--     let (parameters', parameters) = hunappendFD parameters''
--         lstmLayer' = replaceParameters lstmLayer parameters
--         lstmLayerStack' = replaceParameters @(LSTMLayerStack inputSize hiddenSize (numLayers - 1) directionality dtype device) lstmLayerStack parameters'
--      in LSTMLayerK lstmLayer' lstmLayerStack'


        









-- instance
--   ( 1 <= numLayers,
--     (2 <=? numLayers) ~ flag,
--     LSTMLayerStackParameterized flag inputSize hiddenSize numLayers directionality dtype device
--   ) =>
--   Parameterized (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device)
--   where
--   type
--     Parameters (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device) =
--       LSTMLayerStackParameters (2 <=? numLayers) inputSize hiddenSize numLayers directionality dtype device
--   flattenParameters = lstmLayerStackFlattenParameters (Proxy :: Proxy flag)
--   replaceParameters = lstmLayerStackReplaceParameters (Proxy :: Proxy flag)

-- class LSTMLayerStackRandomizable (flag :: Bool) inputSize hiddenSize numLayers directionality dtype device where
--   lstmLayerStackSample ::
--     Proxy flag ->
--     LSTMLayerStackSpec inputSize hiddenSize numLayers directionality dtype device ->
--     IO (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device)

-- instance
--   ( A.Randomizable
--       (LSTMLayerSpec inputSize hiddenSize directionality dtype device)
--       (LSTMLayer inputSize hiddenSize directionality dtype device)
--   ) =>
--   LSTMLayerStackRandomizable 'False inputSize hiddenSize 1 directionality dtype device
--   where
--   lstmLayerStackSample _ _ = LSTMLayer1 <$> (sample $ LSTMLayerSpec @inputSize @hiddenSize @directionality @dtype @device)

-- instance
--   ( 1 <= numLayers,
--     A.Randomizable
--       (LSTMLayerSpec (hiddenSize * NumberOfDirections directionality) hiddenSize directionality dtype device)
--       (LSTMLayer (hiddenSize * NumberOfDirections directionality) hiddenSize directionality dtype device),
--     A.Randomizable
--       (LSTMLayerStackSpec inputSize hiddenSize (numLayers - 1) directionality dtype device)
--       (LSTMLayerStack inputSize hiddenSize (numLayers - 1) directionality dtype device)
--   ) =>
--   LSTMLayerStackRandomizable 'True inputSize hiddenSize numLayers directionality dtype device
--   where
--   lstmLayerStackSample _ _ =
--     LSTMLayerK
--       <$> (sample $ LSTMLayerSpec @(hiddenSize * NumberOfDirections directionality) @hiddenSize @directionality @dtype @device)
--       <*> ( sample
--               @(LSTMLayerStackSpec inputSize hiddenSize (numLayers - 1) directionality dtype device)
--               @(LSTMLayerStack inputSize hiddenSize (numLayers - 1) directionality dtype device)
--               $ LSTMLayerStackSpec
--           )

-- instance
--   ( 1 <= numLayers,
--     (2 <=? numLayers) ~ flag,
--     RandDTypeIsValid device dtype,
--     KnownDType dtype,
--     KnownDevice device,
--     LSTMLayerStackRandomizable flag inputSize hiddenSize numLayers directionality dtype device
--   ) =>
--   Randomizable
--     (LSTMLayerStackSpec inputSize hiddenSize numLayers directionality dtype device)
--     (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device)
--   where
--   sample = lstmLayerStackSample (Proxy :: Proxy flag)

-- instance A.Parameterized (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device) where
--   flattenParameters (LSTMLayer1 layer) =
--     A.flattenParameters layer
--   flattenParameters (LSTMLayerK stack layer) =
--     A.flattenParameters stack
--       ++ A.flattenParameters layer
--   _replaceParameters (LSTMLayer1 layer) = do
--     layer' <- A._replaceParameters layer
--     return $ LSTMLayer1 layer'
--   _replaceParameters (LSTMLayerK stack layer) = do
--     stack' <- A._replaceParameters stack
--     layer' <- A._replaceParameters layer
--     return $ LSTMLayerK stack' layer'


-- newtype
--   LSTMSpec
--     (inputSize :: Nat)
--     (hiddenSize :: Nat)
--     (numLayers :: Nat)
--     (directionality :: RNNDirectionality)
--     (dtype :: D.DType)
--     (device :: (D.DeviceType, Nat))
--   = LSTMSpec DropoutSpec
--   deriving (Show, Generic)

-- data
--   LSTM
--     (inputSize :: Nat)
--     (hiddenSize :: Nat)
--     (numLayers :: Nat)
--     (directionality :: RNNDirectionality)
--     (dtype :: D.DType)
--     (device :: (D.DeviceType, Nat))
--   where
--   LSTM ::
--     (1 <= numLayers) =>
--     { lstm_layer_stack :: LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device,
--       lstm_dropout :: Dropout
--     } ->
--     LSTM inputSize hiddenSize numLayers directionality dtype device

-- deriving instance Show (LSTM inputSize hiddenSize numLayers directionality dtype device)

-- instance
--   (1 <= numLayers) =>
--   Generic (LSTM inputSize hiddenSize numLayers directionality dtype device)
--   where
--   type
--     Rep (LSTM inputSize hiddenSize numLayers directionality dtype device) =
--       Rec0 (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device)
--         :*: Rec0 Dropout
--   from (LSTM {..}) = K1 lstm_layer_stack :*: K1 lstm_dropout
--   to (K1 layerStack :*: K1 dropout) = LSTM layerStack dropout

-- instance
--   ( 1 <= numLayers,
--     Parameterized (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device),
--     HAppendFD
--       (Parameters (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device))
--       (Parameters Dropout)
--       ( Parameters (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device)
--           ++ Parameters Dropout
--       )
--   ) =>
--   Parameterized (LSTM inputSize hiddenSize numLayers directionality dtype device)

-- -- TODO: when we have cannonical initializers do this correctly:
-- -- https://github.com/pytorch/pytorch/issues/9221
-- -- https://discuss.pytorch.org/t/initializing-rnn-gru-and-lstm-correctly/23605

-- instance A.Parameterized (LSTM inputSize hiddenSize numLayers directionality dtype device) where
--   flattenParameters LSTM {..} = A.flattenParameters lstm_layer_stack
--   _replaceParameters LSTM {..} = do
--     lstm_layer_stack' <- A._replaceParameters lstm_layer_stack
--     return $
--       LSTM
--         { lstm_layer_stack = lstm_layer_stack',
--           ..
--         }

-- -- | Helper to do xavier uniform initializations on weight matrices and
-- -- orthagonal initializations for the gates. (When implemented.)
-- xavierUniformLSTM ::
--   forall device dtype hiddenSize featureSize.
--   ( KnownDType dtype,
--     KnownNat hiddenSize,
--     KnownNat featureSize,
--     KnownDevice device,
--     RandDTypeIsValid device dtype
--   ) =>
--   IO (Tensor device dtype '[4 * hiddenSize, featureSize])
-- xavierUniformLSTM = do
--   init <- randn :: IO (Tensor device dtype '[4 * hiddenSize, featureSize])
--   UnsafeMkTensor
--     <$> xavierUniformFIXME
--       (toDynamic init)
--       (5.0 / 3)
--       (shape @device @dtype @'[4 * hiddenSize, featureSize] init)

-- instance
--   ( KnownDType dtype,
--     KnownDevice device,
--     KnownNat inputSize,
--     KnownNat hiddenSize,
--     KnownNat (NumberOfDirections directionality),
--     RandDTypeIsValid device dtype,
--     A.Randomizable
--       (LSTMLayerStackSpec inputSize hiddenSize numLayers directionality dtype device)
--       (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device),
--     1 <= numLayers
--   ) =>
--   A.Randomizable
--     (LSTMSpec inputSize hiddenSize numLayers directionality dtype device)
--     (LSTM inputSize hiddenSize numLayers directionality dtype device)
--   where
--   sample (LSTMSpec dropoutSpec) =
--     LSTM
--       <$> A.sample (LSTMLayerStackSpec @inputSize @hiddenSize @numLayers @directionality @dtype @device)
--       <*> A.sample dropoutSpec


-- lstmForward ::
--   forall
--     shapeOrder
--     batchSize
--     seqLen
--     directionality
--     initialization
--     numLayers
--     inputSize
--     outputSize
--     hiddenSize
--     inputShape
--     outputShape
--     hxShape
--     parameters
--     tensorParameters
--     dtype
--     device.
--   ( KnownNat (NumberOfDirections directionality),
--     KnownNat numLayers,
--     KnownNat batchSize,
--     KnownNat hiddenSize,
--     KnownRNNShapeOrder shapeOrder,
--     KnownRNNDirectionality directionality,
--     outputSize ~ (hiddenSize * NumberOfDirections directionality),
--     inputShape ~ RNNShape shapeOrder seqLen batchSize inputSize,
--     outputShape ~ RNNShape shapeOrder seqLen batchSize outputSize,
--     hxShape ~ '[numLayers * NumberOfDirections directionality, batchSize, hiddenSize],
--     parameters ~ Parameters (LSTM inputSize hiddenSize numLayers directionality dtype device),
--     Parameterized (LSTM inputSize hiddenSize numLayers directionality dtype device),
--     tensorParameters ~ LSTMR inputSize hiddenSize numLayers directionality dtype device,
--     ATen.Castable (HList tensorParameters) [D.ATenTensor],
--     HMap' ToDependent parameters tensorParameters
--   ) =>
--   Bool ->
--   LSTMWithInit
--     inputSize
--     hiddenSize
--     numLayers
--     directionality
--     initialization
--     dtype
--     device ->
--   Tensor device dtype inputShape ->
--   ( Tensor device dtype outputShape,
--     Tensor device dtype hxShape,
--     Tensor device dtype hxShape
--   )
-- lstmForward dropoutOn (LSTMWithConstInit lstmModel@(LSTM _ (Dropout dropoutProb)) cc hc) input =
--   lstm
--     @shapeOrder
--     @directionality
--     @numLayers
--     @seqLen
--     @batchSize
--     @inputSize
--     @outputSize
--     @hiddenSize
--     @inputShape
--     @outputShape
--     @hxShape
--     @tensorParameters
--     @dtype
--     @device
--     (hmap' ToDependent . flattenParameters $ lstmModel)
--     dropoutProb
--     dropoutOn
--     (cc', hc')
--     input
--   where
--     cc' =
--       reshape @hxShape
--         . expand
--           @'[batchSize, numLayers * NumberOfDirections directionality, hiddenSize]
--           False -- TODO: What does the bool do?
--         $ cc
--     hc' =
--       reshape @hxShape
--         . expand
--           @'[batchSize, numLayers * NumberOfDirections directionality, hiddenSize]
--           False -- TODO: What does the bool do?
--         $ hc
-- lstmForward dropoutOn (LSTMWithLearnedInit lstmModel@(LSTM _ (Dropout dropoutProb)) cc hc) input =
--   lstm
--     @shapeOrder
--     @directionality
--     @numLayers
--     @seqLen
--     @batchSize
--     @inputSize
--     @outputSize
--     @hiddenSize
--     @inputShape
--     @outputShape
--     @hxShape
--     @tensorParameters
--     @dtype
--     @device
--     (hmap' ToDependent . flattenParameters $ lstmModel)
--     dropoutProb
--     dropoutOn
--     (cc', hc')
--     input
--   where
--     cc' =
--       reshape @hxShape
--         . expand
--           @'[batchSize, numLayers * NumberOfDirections directionality, hiddenSize]
--           False -- TODO: What does the bool do?
--         . toDependent
--         $ cc
--     hc' =
--       reshape @hxShape
--         . expand
--           @'[batchSize, numLayers * NumberOfDirections directionality, hiddenSize]
--           False -- TODO: What does the bool do?
--         . toDependent
--         $ hc
