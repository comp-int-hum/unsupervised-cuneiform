module UnsupervisedCuneiform.CNN ( CNN(..)
                                 , CNNSpec(..)
                                 ) where

import System.IO.Unsafe
import GHC.TypeLits
import GHC.Types
import GHC.Generics
import Data.Proxy
import Torch.Typed
import Torch.Typed.Auxiliary


data CNNSpec (channelSizes :: [Nat]) (kernelSizes :: [(Nat, Nat)]) (dtype :: DType) (device :: (DeviceType, Nat)) = CNNSpec
  deriving (Eq, Show)


data CNN (channelSizes :: [Nat]) (kernelSizes :: [(Nat, Nat)]) (dtype :: DType) (device :: (DeviceType, Nat)) where
  CNN ::
    { layers :: HList (InstantiateTypesR channelSizes kernelSizes dtype device Conv2d)
    } -> CNN channelSizes kernelSizes dtype device
  deriving (Generic)

instance Show (CNN channelSizes kernelSizes dtype device) where
  show _ = "CNN"
--deriving instance ( Show ( HList (InstantiateTypesR channelSizes kernelSizes dtype device Conv2d) ) ) => Show (CNN channelSizes kernelSizes dtype device)


class InstantiateConv2dSpecsF (channelSizes :: [Nat]) (kernelSizes :: [(Nat, Nat)]) (dtype :: DType) (device :: (DeviceType, Nat)) where
  instantiateConv2dSpecsF :: HList (InstantiateTypesR channelSizes kernelSizes dtype device Conv2dSpec)


-- not sure why this instance is necessary...
instance InstantiateConv2dSpecsF '[] ks dtype device where
  instantiateConv2dSpecsF = undefined
  

instance InstantiateConv2dSpecsF (o ': '[]) ys dtype device where
  instantiateConv2dSpecsF = HNil


instance ( InstantiateConv2dSpecsF cs ks dtype device
         , tp ~ Conv2dSpec i o w h dtype device
         , InstantiateConv2dSpecsF (o ': cs) ks dtype device
         ) => InstantiateConv2dSpecsF ( i ':  o ': cs) ( '( w, h ) ': ks) dtype device where
  instantiateConv2dSpecsF = (Conv2dSpec :: tp) :. (instantiateConv2dSpecsF @(o ': cs) @ks @dtype @device)


type family InstantiateTypesR
     (channelSizes :: [Nat])
     (kernelSizes :: [(Nat, Nat)])     
     (dtype :: DType)
     (device :: (DeviceType, Nat))
     (tp :: Nat -> Nat -> Nat -> Nat -> DType -> (DeviceType, Nat) -> Type)
     :: [Type] where
  InstantiateTypesR (o ': '[]) _ _ _ _ = '[]
  InstantiateTypesR ( i ': o ': xs) ( '( w, h ) : ys) dtype device tp = (tp i o w h dtype device) ': (InstantiateTypesR (o ': xs) ys dtype device tp)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , Randomizable (HList (InstantiateTypesR channelSizes kernelSizes dtype device Conv2dSpec) ) (HList (InstantiateTypesR channelSizes kernelSizes dtype device Conv2d) )
  , InstantiateConv2dSpecsF channelSizes kernelSizes dtype device
  ) =>
  Randomizable (CNNSpec channelSizes kernelSizes dtype device) (CNN channelSizes kernelSizes dtype device) where
  sample _ = CNN <$> sample (instantiateConv2dSpecsF @channelSizes @kernelSizes @dtype @device)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , Randomizable (HList (InstantiateTypesR channelSizes kernelSizes dtype device Conv2dSpec) ) (HList (InstantiateTypesR channelSizes kernelSizes dtype device Conv2d) )
  , InstantiateConv2dSpecsF channelSizes kernelSizes dtype device
  ) =>
  Randomizable (Proxy (CNNSpec channelSizes kernelSizes dtype device)) (CNN channelSizes kernelSizes dtype device) where
  sample _ = CNN <$> sample (instantiateConv2dSpecsF @channelSizes @kernelSizes @dtype @device)


instance
  ( layers ~ (InstantiateTypesR channelSizes kernelSizes dtype device Conv2d)
  , Parameterized (HList layers)
  ) => Parameterized (CNN channelSizes kernelSizes dtype device)


instance
  ( All KnownNat restC
  , All KnownNat '[ batchSize
                  , iC
                  , oC
                  , i1
                  , i2
                  , o1
                  , o2
                  , oCF
                  , k1
                  , k2
                  , Torch.Typed.Auxiliary.Fst stride
                  , Torch.Typed.Auxiliary.Snd stride
                  , Torch.Typed.Auxiliary.Fst padding
                  , Torch.Typed.Auxiliary.Snd padding
                  ]
  , stride ~ '(1, 1)
  , padding ~ '( Div (k1 - 1) 2, Div (k2 - 1) 2)
  , StandardFloatingPointDTypeValidation device dtype
  --, shapeC ~ (i ': i' ': i'' ': restC)
  , (oCF ': init) ~ (Reverse (oC' ': restC))
  , BasicArithmeticDTypeIsValid device dtype
  , ComparisonDTypeIsValid device dtype
  , KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , i1 ~ o1
  , i2 ~ o2
  , o1F ~ o1
  , o2F ~ o2
  , HasForward
    (CNN (oC ': oC' ': restC) restK dtype device)
    (Tensor device dtype '[batchSize, oC, i1, i2])
    (Tensor device dtype '[batchSize, oCF, o1F, o2F])
  , HasForward
    (Conv2d iC oC k1 k2 dtype device)
    (Tensor device dtype '[batchSize, iC, i1, i2], Proxy stride, Proxy padding)
    (Tensor device dtype '[batchSize, oC, o1, o2])
  , ConvSideCheck i1 k1 (Torch.Typed.Auxiliary.Fst stride) (Torch.Typed.Auxiliary.Fst padding) o1
  , ConvSideCheck i2 k2 (Torch.Typed.Auxiliary.Snd stride) (Torch.Typed.Auxiliary.Snd padding) o2
  , 1 ~ Mod k1 2
  , 1 ~ Mod k2 2  
  ) =>
  HasForward
  (CNN (iC ': oC ': oC' ': restC) ( '(k1, k2) ': restK) dtype device)
  (Tensor device dtype '[batchSize, iC, i1, i2])
  (Tensor device dtype '[batchSize, oCF, o1F, o2F]) where
  forward model = unsafePerformIO . forwardStoch model
  forwardStoch (CNN (l :. restL)) inp = do
    let nextModel = CNN restL :: CNN (oC ': oC' ': restC) restK dtype device
    inp' <- forwardStoch l (inp, Proxy :: Proxy stride, Proxy :: Proxy padding) :: IO (Tensor device dtype '[batchSize, oC, o1, o2])
    forwardStoch nextModel inp'
      

instance
  ( All KnownNat '[ batchSize
                  , iC
                  , oC
                  , k1
                  , k2
                  , i1
                  , i2
                  , o1
                  , o2
                  , Torch.Typed.Auxiliary.Fst stride
                  , Torch.Typed.Auxiliary.Snd stride
                  , Torch.Typed.Auxiliary.Fst padding
                  , Torch.Typed.Auxiliary.Snd padding
                  ]
  , stride ~ '(1, 1)
  , padding ~ '( Div (k1 - 1) 2, Div (k2 - 1) 2)
  , StandardFloatingPointDTypeValidation device dtype
  , BasicArithmeticDTypeIsValid device dtype
  , ComparisonDTypeIsValid device dtype
  , KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , i1 ~ o1
  , i2 ~ o2
  , HasForward
    (Conv2d iC oC k1 k2 dtype device)
    (Tensor device dtype '[batchSize, iC, i1, i2], Proxy stride, Proxy padding)
    (Tensor device dtype '[batchSize, oC, o1, o2])
  , ConvSideCheck i1 k1 (Torch.Typed.Auxiliary.Fst stride) (Torch.Typed.Auxiliary.Fst padding) o1
  , ConvSideCheck i2 k2 (Torch.Typed.Auxiliary.Snd stride) (Torch.Typed.Auxiliary.Snd padding) o2
  ) =>
  HasForward
  (CNN (iC ': oC ': '[]) ( '(k1, k2) ': '[]) dtype device)
  (Tensor device dtype '[batchSize, iC, i1, i2])
  (Tensor device dtype '[batchSize, oC, o1, o2]) where
  forward model = unsafePerformIO . forwardStoch model
  forwardStoch (CNN (l :. HNil)) inp = forwardStoch l (inp, Proxy :: Proxy stride, Proxy :: Proxy padding)
