module UnsupervisedCuneiform.AutoEncoder ( AutoEncoder(..)
                                         , AutoEncoderSpec(..)
                                         ) where

import System.IO.Unsafe
import GHC.TypeLits
import GHC.Types
import GHC.Generics
import Data.Proxy
import Torch.Typed
import UnsupervisedCuneiform.MLP


data AutoEncoderSpec
  (inputSize :: Nat)
  (encoderShape :: [Nat])
  (bottleneckSize :: Nat)
  (decoderShape :: [Nat])
  (dtype :: DType)
  (device :: (DeviceType, Nat)) = AutoEncoderSpec
  deriving (Eq, Show)


data AutoEncoder
  (inputSize :: Nat)
  (encoderShape :: [Nat])
  (bottleneckSize :: Nat)
  (decoderShape :: [Nat])
  (dtype :: DType)
  (device :: (DeviceType, Nat)) where
  AutoEncoder ::
    { encoder :: MLP (inputSize ': (Reverse (bottleneckSize ': (Reverse encoderShape)))) dtype device
    , decoder :: MLP (bottleneckSize ': (Reverse (inputSize ': (Reverse decoderShape)))) dtype device
    } -> AutoEncoder inputSize encoderShape bottleneckSize decoderShape dtype device
  deriving (Generic)

instance Show (AutoEncoder inputSize encoderShape bottleneckSize decoderShape dtype device) where
  show _ = "AutoEncoder"
--deriving instance ( Show ( HList (InstantiateTypesR shape dtype device Linear) ) ) => Show (AutoEncoder inputSize encoderShape bottleneckSize decoderShape dtype device)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , Randomizable (MLPSpec shape1 dtype device) (MLP shape1 dtype device)
  , Randomizable (MLPSpec shape2 dtype device) (MLP shape2 dtype device)
  , shape1 ~ (inputSize ': (Reverse (bottleneckSize ': (Reverse encoderShape))))
  , shape2 ~ (bottleneckSize ': (Reverse (inputSize ': (Reverse decoderShape))))
  ) =>
  Randomizable (AutoEncoderSpec inputSize encoderShape bottleneckSize decoderShape dtype device) (AutoEncoder inputSize encoderShape bottleneckSize decoderShape dtype device) where
  sample _ = AutoEncoder <$> sample (MLPSpec :: MLPSpec shape1 dtype device) <*> sample (MLPSpec :: MLPSpec shape2 dtype device)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , Randomizable (MLPSpec shape1 dtype device) (MLP shape1 dtype device)
  , Randomizable (MLPSpec shape2 dtype device) (MLP shape2 dtype device)
  , shape1 ~ (inputSize ': (Reverse (bottleneckSize ': (Reverse encoderShape))))
  , shape2 ~ (bottleneckSize ': (Reverse (inputSize ': (Reverse decoderShape))))
  ) =>
  Randomizable (Proxy (AutoEncoderSpec inputSize encoderShape bottleneckSize decoderShape dtype device)) (AutoEncoder inputSize encoderShape bottleneckSize decoderShape dtype device) where
  sample _ = AutoEncoder <$> sample (MLPSpec :: MLPSpec shape1 dtype device) <*> sample (MLPSpec :: MLPSpec shape2 dtype device)


instance ( Parameterized dec
         , Parameterized enc
         , layers1 ~ (InstantiateTypesR shape1 dtype device Linear)
         , layers2 ~ (InstantiateTypesR shape2 dtype device Linear)
         , enc ~ MLP shape1 dtype device
         , dec ~ MLP shape2 dtype device
         , All KnownNat shape1
         , All KnownNat shape2
         , All KnownNat '[inputSize, bottleneckSize]
         , All KnownNat encoderShape
         , All KnownNat decoderShape
         , Parameterized (HList layers1)
         , Parameterized (HList layers2)
         , shape1 ~ (inputSize ': (Reverse (bottleneckSize ': (Reverse encoderShape))))
         , shape2 ~ (bottleneckSize ': (Reverse (inputSize ': (Reverse decoderShape))))
         , HAppendFD (Parameters enc) (Parameters dec) ((Parameters enc) ++ (Parameters dec))
         ) => Parameterized (AutoEncoder inputSize encoderShape bottleneckSize decoderShape dtype device)


instance
  ( All KnownNat '[batchSize, inputSize, bottleneckSize]
  , All KnownNat (encoderShape ++ decoderShape)
  , StandardFloatingPointDTypeValidation device dtype
  , BasicArithmeticDTypeIsValid device dtype
  , ComparisonDTypeIsValid device dtype
  , KnownDType dtype
  , KnownDevice device
  , shape1 ~ (inputSize ': (Reverse (bottleneckSize ': (Reverse encoderShape))))
  , shape2 ~ (bottleneckSize ': (Reverse (inputSize ': (Reverse decoderShape))))
  , enc ~ MLP shape1 dtype device
  , dec ~ MLP shape2 dtype device  
  , RandDTypeIsValid device dtype
  , HasForward enc (Tensor device dtype '[batchSize, inputSize]) (Tensor device dtype '[batchSize, bottleneckSize])
  , HasForward dec (Tensor device dtype '[batchSize, bottleneckSize]) (Tensor device dtype '[batchSize, outputSize])  
  , outputSize ~ inputSize
  ) =>
  HasForward (AutoEncoder inputSize encoderShape bottleneckSize decoderShape dtype device) (Tensor device dtype '[batchSize, inputSize]) (Tensor device dtype '[batchSize, bottleneckSize], Tensor device dtype '[batchSize, outputSize]) where
  forward model = unsafePerformIO . forwardStoch model
  forwardStoch (AutoEncoder enc dec) inp = do
    emb <- forwardStoch enc inp
    recon <- forwardStoch dec emb
    return (emb, recon)
