module UnsupervisedCuneiform.MLP ( MLP(..)
                                 , MLPSpec(..)
                                 , InstantiateTypesR(..)
                                 ) where

import System.IO.Unsafe
import GHC.TypeLits
import GHC.Types
import GHC.Generics
import Data.Proxy
import Torch.Typed hiding (FoldLayers)


type Layer input output dtype device = Linear input output dtype device


data MLPSpec (shape :: [Nat]) (dtype :: DType) (device :: (DeviceType, Nat)) = MLPSpec
  deriving (Eq, Show)


data MLP (shape :: [Nat]) (dtype :: DType) (device :: (DeviceType, Nat)) where
  MLP ::
    { layers :: HList (InstantiateTypesR shape dtype device Linear)
    } -> MLP shape dtype device
  deriving (Generic)

instance Show (MLP shape dtype device) where
  show _ = "MLP"

--deriving instance ( Show ( HList (InstantiateTypesR shape dtype device Linear) ) ) => Show (MLP shape dtype device)


class InstantiateLinearSpecsF (shape :: [Nat]) (dtype :: DType) (device :: (DeviceType, Nat)) where
  instantiateLinearSpecsF :: HList (InstantiateTypesR shape dtype device LinearSpec)


-- not sure why this instance is necessary...
instance InstantiateLinearSpecsF '[] dtype device where
  instantiateLinearSpecsF = undefined
  

instance InstantiateLinearSpecsF (o ': '[]) dtype device where
  instantiateLinearSpecsF = HNil


instance ( InstantiateLinearSpecsF xs dtype device
         , tp ~ LinearSpec i o dtype device
         , InstantiateLinearSpecsF (o ': xs) dtype device
         ) => InstantiateLinearSpecsF ( i ':  o ': xs) dtype device where
  instantiateLinearSpecsF = (LinearSpec :: tp) :. (instantiateLinearSpecsF @(o ': xs) @dtype @device)


type family InstantiateTypesR
     (shape :: [Nat])
     (dtype :: DType)
     (device :: (DeviceType, Nat))
     (tp :: Nat -> Nat -> DType -> (DeviceType, Nat) -> Type)
     :: [Type] where
  InstantiateTypesR (o ': '[]) _ _ _ = '[]
  InstantiateTypesR ( i ': o ': xs) dtype device tp = (tp i o dtype device) ': (InstantiateTypesR (o ': xs) dtype device tp)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , Randomizable (HList (InstantiateTypesR shape dtype device LinearSpec) ) (HList (InstantiateTypesR shape dtype device Linear) )
  , InstantiateLinearSpecsF shape dtype device
  ) =>
  Randomizable (MLPSpec shape dtype device) (MLP shape dtype device) where
  sample _ = MLP <$> sample (instantiateLinearSpecsF @shape @dtype @device)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , Randomizable (HList (InstantiateTypesR shape dtype device LinearSpec) ) (HList (InstantiateTypesR shape dtype device Linear) )
  , InstantiateLinearSpecsF shape dtype device
  ) =>
  Randomizable (Proxy (MLPSpec shape dtype device)) (MLP shape dtype device) where
  sample _ = MLP <$> sample (instantiateLinearSpecsF @shape @dtype @device)


instance
  ( layers ~ (InstantiateTypesR shape dtype device Linear)
  , Parameterized (HList layers)
  ) => Parameterized (MLP shape dtype device)


instance
  ( All KnownNat rest
  , All KnownNat '[batchSize, i, i', i'', o]
  , StandardFloatingPointDTypeValidation device dtype
  , shape ~ (i ': i' ': i'' ': rest)
  , (o ': init) ~ (Reverse shape)
  , BasicArithmeticDTypeIsValid device dtype
  , ComparisonDTypeIsValid device dtype
  , KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , HasForward (MLP (i' ': i'' ': rest) dtype device) (Tensor device dtype '[batchSize, i']) (Tensor device dtype '[batchSize, o])
  ) =>
  HasForward (MLP (i ': i' ': i'' ': rest) dtype device) (Tensor device dtype '[batchSize, i]) (Tensor device dtype '[batchSize, o]) where
  forward model = unsafePerformIO . forwardStoch model
  forwardStoch (MLP (l :. rest)) inp = do
    let nextModel = MLP rest :: MLP (i' ': i'' ': rest) dtype device
    inp' <- forwardStoch l inp
    forwardStoch nextModel inp'
      

instance
  ( All KnownNat '[batchSize, i, o]
  , StandardFloatingPointDTypeValidation device dtype
  , BasicArithmeticDTypeIsValid device dtype
  , ComparisonDTypeIsValid device dtype
  , KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  ) =>
  HasForward (MLP (i ': o ': '[]) dtype device) (Tensor device dtype '[batchSize, i]) (Tensor device dtype '[batchSize, o]) where
  forward model = unsafePerformIO . forwardStoch model
  forwardStoch (MLP (l :. HNil)) inp = forwardStoch l inp
