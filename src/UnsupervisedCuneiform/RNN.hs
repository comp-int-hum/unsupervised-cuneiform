module UnsupervisedCuneiform.RNN ( RNN(..)
                                 , RNNSpec(..)
                                 ) where

import System.IO.Unsafe
import GHC.TypeLits
import GHC.Types
import GHC.Generics
import Data.Proxy
import Torch.Typed hiding (FoldLayers)



data RNNSpec (embeddingSize :: Nat) (hiddenSize :: Nat) (dtype :: DType) (device :: (DeviceType, Nat)) = RNNSpec
  deriving (Eq, Show)


data RNN (embeddingSize :: Nat) (hiddenSize :: Nat) (dtype :: DType) (device :: (DeviceType, Nat)) where
  RNN ::
    { lstm :: LSTMWithInit embeddingSize hiddenSize 1 Unidirectional ConstantInitialization dtype device
    } -> RNN embeddingSize hiddenSize dtype device
  deriving (Generic)

instance Show (RNN embeddingSize hiddenSize dtype device) where
  show _ = "RNN"

--deriving instance Show (RNN inputSize hiddenSize dtype device)

instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , All KnownNat '[ embeddingSize, hiddenSize ]
  ) =>
  Randomizable (RNNSpec embeddingSize hiddenSize dtype device) (RNN embeddingSize hiddenSize dtype device) where
  sample _ = RNN <$> sample (LSTMWithZerosInitSpec (LSTMSpec (DropoutSpec 0.0) :: LSTMSpec embeddingSize hiddenSize 1 Unidirectional dtype device))


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , All KnownNat '[ embeddingSize, hiddenSize ]
  ) =>
  Randomizable (Proxy (RNNSpec embeddingSize hiddenSize dtype device)) (RNN embeddingSize hiddenSize dtype device) where
  sample _ = RNN <$> sample (LSTMWithZerosInitSpec (LSTMSpec (DropoutSpec 0.0) :: LSTMSpec embeddingSize hiddenSize 1 Unidirectional dtype device))


instance Parameterized (RNN embeddingSize hiddenSize dtype device)


instance
  ( All KnownNat '[length, embeddingSize, hiddenSize, batchSize]
  , KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  ) =>
  HasForward
  (RNN embeddingSize hiddenSize dtype device)
  (Tensor device dtype '[batchSize, length, embeddingSize])
  (Tensor device dtype '[batchSize, length, hiddenSize]) where
  forward model = unsafePerformIO . forwardStoch model
  forwardStoch (RNN lstm) inp = do
    (x, _, _) <- pure $ lstmForward @'BatchFirst False lstm inp
    return x
