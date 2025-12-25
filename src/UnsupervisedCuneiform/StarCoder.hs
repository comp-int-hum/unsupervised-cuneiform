module UnsupervisedCuneiform.StarCoder ( StarCoder(..)
                                       , StarCoderSpec(..)                                       
                                       , DType(..)
                                       , DeviceType(..)
                                       , Randomizable(..)
                                       , DomainInputR(..)
                                       , SampleInputF(..)
                                       , Configuration(..)
                                       , Instantiate(..)
                                       , InstType(..)
                                       , InstSpec(..)
                                       --, Get(..)
                                       ) where

import System.IO.Unsafe
import Data.Proxy
import GHC.TypeLits
import GHC.Types hiding (TyCon)
import GHC.Generics
import GHC.TypeError as E
import Data.Tagged
import Data.Kind
import Torch.Typed hiding (Scalar, Apply, Size, Embedding, EmbeddingSpec)
import qualified Torch as T
import UnsupervisedCuneiform.MLP
import UnsupervisedCuneiform.AutoEncoder
import UnsupervisedCuneiform.RNN
import UnsupervisedCuneiform.CNN
import UnsupervisedCuneiform.Embedding
import UnsupervisedCuneiform.Trie (Trie, MatchR(..), PathComp(..), Value(..), type (^#), type (^$), type (^*#), type (^*##))
import UnsupervisedCuneiform.Domain (Domain, PropertyList)
import UnsupervisedCuneiform.Utils ( Categorical
                                   , Image
                                   , Scalar
                                   , Text
                                   , Related(..)                                   
                                   )

type Configuration = Trie
type MaxEntityCount = 20
type BottleneckSize = 128 --Left 128 :: (Either Nat *)
type EncoderShape = '[512, 256]
type DecoderShape = '[256, 512]
type TextEmbeddingSize = 2 --128
type TokenEmbeddingSize = 2 --32
type ScalarEmbeddingSize = 2 --5
type CategoricalEmbeddingSize = 2 --64
type HiddenSize = 3 --64
type Depth = 3
type BatchSize = 1 --50
type ImageWidth = 1024
type ImageHeight = 768
type ImageEmbeddingSize = 48
type EntityCount = 5


data StarCoder
  (conf :: Configuration)
  (entityTypes :: [ (Symbol, PropertyList) ])
  (depth :: Nat)
  (dtype :: DType)
  (device :: (DeviceType, Nat)) where
  StarCoder ::
    { encoders :: HList (Concat (Map (MkEncoders conf) entityTypes) )
    , autoEncoders :: HList (Concat (Map (MkAutoEncoders dtype device) entityTypes) )
    , decoders :: HList (Concat (Map (MkDecoders conf dtype device) entityTypes) )
    , summarizers :: HList (Concat (Map (MkSummarizers conf) entityTypes) )
    } -> StarCoder conf entityTypes depth dtype device
    deriving (Generic)


data StarCoderSpec
  (conf :: Configuration)
  (entityTypes :: Domain )
  (depth :: Nat)
  (dtype :: DType)
  (device :: (DeviceType, Nat)) = StarCoderSpec deriving (Show, Eq)


deriving instance ( Show (HList (Concat (Map (MkEncoders conf) entityTypes) ) )
                  , Show (HList (Concat (Map (MkAutoEncoders dtype device) entityTypes) ) )
                  , Show (HList (Concat (Map (MkDecoders conf dtype device) entityTypes) ) )
                  , Show (HList (Concat (Map (MkSummarizers conf) entityTypes) ) )
                  ) => Show (StarCoder conf entityTypes depth dtype device)


instance
  ( Parameterized (HList (Concat (Map (MkEncoders conf) entityTypes) ) )
  , Parameterized (HList (Concat (Map (MkAutoEncoders dtype device) entityTypes) ) )
  , Parameterized (HList (Concat (Map (MkDecoders conf dtype device) entityTypes) ) )
  , Parameterized (HList (Concat (Map (MkSummarizers conf) entityTypes) ) )  
  , a ~ (Parameters (HList (Concat (Map (MkEncoders conf) entityTypes) ) ) )
  , b ~ (Parameters (HList (Concat (Map (MkAutoEncoders dtype device) entityTypes) ) ) )
  , c ~ (Parameters (HList (Concat (Map (MkDecoders conf dtype device) entityTypes) ) ) )
  , d ~ (Parameters (HList (Concat (Map (MkSummarizers conf) entityTypes) ) ) )
  , HAppendFD a b (a ++ b)
  , HAppendFD c d (c ++ d)
  , HAppendFD (a ++ b) (c ++ d) ((a ++ b) ++ (c ++ d))
  ) => Parameterized (StarCoder conf entityTypes depth dtype device)


type family DomainInputR
  ( entityTypes :: [ (Symbol, PropertyList ) ] )
  ( dtype :: DType )
  ( device :: (DeviceType, Nat) )
  where
  DomainInputR '[] dtype device = '[]
  DomainInputR ( '(e, ps) ': rest) dtype device = (EntityInputR e ps dtype device) ++ (DomainInputR rest dtype device)

type family EntityInputR
  ( entity :: Symbol )
  ( properties :: PropertyList )
  ( dtype :: DType )
  ( device :: (DeviceType, Nat) )
  :: [Type] where

  EntityInputR _ '[] _ _ = '[]
  EntityInputR e ( '( p, t ) ': rest) dtype device = (Tagged '( e, p ) (PropertyInputR t dtype device) ) ': (EntityInputR e rest dtype device)




type family PropertyInputR
  ( property :: Type )
  ( dtype :: DType )
  ( device :: (DeviceType, Nat) )
  :: Type where

  PropertyInputR Scalar dtype device = Tensor device T.Float '[EntityCount]
  PropertyInputR Text dtype device = Tensor device Int64 '[EntityCount, MaxSequenceLength]
  PropertyInputR Categorical dtype device = Tensor device Int64 '[EntityCount]
  PropertyInputR Image dtype device = Tensor device T.Float '[EntityCount, ImageWidth, ImageHeight, 3]
  PropertyInputR (Related a) dtype device = Tensor device T.Bool '[EntityCount, EntityCount]



type family DomainOutputR
  ( entityTypes :: [ (Symbol, PropertyList ) ] )
  (dtype :: DType)
  (device :: (DeviceType, Nat))
  where
  DomainOutputR '[] dtype device = '[]
  DomainOutputR ( '(e, ps) ': rest) dtype device = (EntityOutputR e ps dtype device) ++ (DomainOutputR rest dtype device)

type family EntityOutputR
  ( entity :: Symbol )
  ( properties :: PropertyList )
  ( dtype :: DType )
  ( device :: (DeviceType, Nat) )
  :: [Type] where

  EntityOutputR _ '[] _ _ = '[]
  EntityOutputR e ( '( p, t ) ': rest) dtype device = (Tagged '( e, p ) (PropertyOutputR t dtype device) ) ': (EntityOutputR e rest dtype device)


type MaxSequenceLength = 512

type family PropertyOutputR
  ( property :: Type )
  ( dtype :: DType )
  ( device :: (DeviceType, Nat) )
  :: Type where

  PropertyOutputR Scalar dtype device = Tensor device T.Float '[EntityCount]
  PropertyOutputR Text dtype device = Tensor device Int64 '[EntityCount, MaxSequenceLength]
  PropertyOutputR Categorical dtype device = Tensor device Int64 '[EntityCount]
  PropertyOutputR Image dtype device = Tensor device T.Float '[EntityCount, ImageWidth, ImageHeight, 3]
  PropertyOutputR (Related a) dtype device = Tensor device T.Bool '[EntityCount, EntityCount]




class SampleInputF (result :: [Type])  where
  sampleInputF :: IO (HList result)


instance
  ( 
  ) =>
  SampleInputF '[] where
  sampleInputF = return HNil


instance
  ( RandDTypeIsValid device T.Float
  , TensorOptions shape T.Float device
  , SampleInputF rest
  ) =>
  SampleInputF ((Tagged t (Tensor device T.Float shape)) ': rest) where
  sampleInputF = do
    n <- rand :: IO (Tensor device T.Float shape)
    --let n = zeros :: Tensor device dtype shape
    ns <- sampleInputF @rest
    return $ (Tagged n :: Tagged t (Tensor device T.Float shape) ) :. ns


instance
  ( RandDTypeIsValid device Int64
  , shape ~ '[ count ]
  , TensorOptions shape Int64 device
  , SampleInputF rest
  ) =>
  SampleInputF ((Tagged t (Tensor device Int64 '[ count ])) ': rest) where
  sampleInputF = do
    let n = zeros :: Tensor device Int64 shape
    ns <- sampleInputF @rest
    return $ (Tagged n :: Tagged t (Tensor device Int64 shape) ) :. ns


instance
  ( RandDTypeIsValid device Int64
  , shape ~ '[ count, max ]
  , TensorOptions shape Int64 device
  , SampleInputF rest
  ) =>
  SampleInputF ((Tagged t (Tensor device Int64 '[ count, max ])) ': rest) where
  sampleInputF = do
    n <- randint 1 10 :: IO (Tensor device Int64 shape)
    ns <- sampleInputF @rest
    return $ (Tagged n :: Tagged t (Tensor device Int64 shape) ) :. ns


instance
  ( RandDTypeIsValid device 'Float
  , TensorOptions shape T.Bool device
  , SampleInputF rest
  , shape ~ Broadcast shape shape
  , ComparisonDTypeIsValid device 'Float
  , TensorOptions shape 'Float device
  ) =>
  SampleInputF ((Tagged t (Tensor device T.Bool shape)) ': rest) where
  sampleInputF = do
    n' <- randn :: IO (Tensor device 'Float shape)
    let ref = full (0.7 :: Float) :: Tensor device 'Float shape
        n = n' >. ref
    ns <- sampleInputF @rest
    return $ (Tagged n :: Tagged t (Tensor device T.Bool shape) ) :. ns


instance
  ( Randomizable spec arch
  ) =>
  Randomizable ( Tagged t spec ) ( Tagged t arch ) where
  sample s = do
    a <- sample (untag s)
    return $ Tagged a


instance
  ( Parameterized arch
  ) => Parameterized (Tagged t arch)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , encArchs ~ (Concat (Map (MkEncoders conf) entityTypes) )
  , encPspecs ~ (Concat (Map (MkEncoderSpecs conf) entityTypes) )
  , decArchs ~ (Concat (Map (MkDecoders conf dtype device) entityTypes) )
  , decPspecs ~ (Concat (Map (MkDecoderSpecs conf dtype device) entityTypes) )  
  , aeArchs ~ (Concat (Map (MkAutoEncoders dtype device) entityTypes) )
  , aePspecs ~ (Concat (Map (MkAutoEncoderSpecs dtype device) entityTypes) )
  , sumArchs ~ (Concat (Map (MkSummarizers conf) entityTypes) )
  , sumPspecs ~ (Concat (Map (MkSummarizerSpecs conf) entityTypes) )    
  , Randomizable (HList encPspecs) (HList encArchs)
  , Randomizable (HList aePspecs) (HList aeArchs)
  , Randomizable (HList decPspecs) (HList decArchs)
  , Randomizable (HList sumPspecs) (HList sumArchs)  
  , InstantiateProxiesF encPspecs
  , InstantiateProxiesF aePspecs
  , InstantiateProxiesF decPspecs
  , InstantiateProxiesF sumPspecs  
  ) =>
  Randomizable (StarCoderSpec conf entityTypes depth dtype device) (StarCoder conf entityTypes depth dtype device) where
  sample _ = StarCoder <$> sample (instantiateProxiesF @encPspecs) <*> sample (instantiateProxiesF @aePspecs) <*> sample (instantiateProxiesF @decPspecs) <*> sample (instantiateProxiesF @sumPspecs)


class InstantiateProxiesF (pspecs :: [Type]) where
  instantiateProxiesF :: HList pspecs


instance InstantiateProxiesF '[] where
  instantiateProxiesF = HNil


instance
  ( InstantiateProxiesF rest
  , x ~ Proxy a
  ) =>
  InstantiateProxiesF ( ( Tagged v x) ': rest) where
  instantiateProxiesF = (Tagged (Proxy :: x) ) :. (instantiateProxiesF @rest)


type family NextItem
  (c :: Either x [x])
  where
    NextItem (Left x) = x
    NextItem (Right (x ': rest)) = x


type family NextSource
  (c :: Either x [x])
  where
    NextSource (Left x) = Left x
    NextSource (Right (x ': rest)) = Right rest


type family SizeSum
  (ps :: [ (Symbol, Type) ])
  :: Nat where
  SizeSum '[] = 0
  SizeSum ( '( _, Text) ': rest ) = TextEmbeddingSize + (SizeSum rest)
  SizeSum ( '( _, Scalar) ': rest ) = ScalarEmbeddingSize + (SizeSum rest)
  SizeSum ( '( _, Image) ': rest ) = ImageEmbeddingSize + (SizeSum rest)
  SizeSum ( '( _, Categorical) ': rest ) = CategoricalEmbeddingSize + (SizeSum rest)
  SizeSum ( '( _, Related a) ': rest ) = 0 + (SizeSum rest)    


data TyFun :: Type -> Type -> Type
data TyCon :: (k1 -> k2) -> (TyFun k1 k2) -> Type
type family Apply (f :: (TyFun k1 k2) -> Type) (x :: k1) :: k2
type instance Apply (TyCon tc) x = tc x

type family Map (f :: (TyFun k1 k2) -> Type) (ls :: [k1]) :: [k2]
type instance Map f '[] = '[]
type instance Map f (h ': t) = (Apply f h) ': (Map f t)

type family Concat (xs :: [[Type]]) :: [Type] where
  Concat '[] = '[]
  Concat (x ': rest) = x ++ (Concat rest)

  
type Instantiate :: forall k. k -> [Value] -> Type

type family Instantiate tc tlist where

  Instantiate tc '[] = tc

  Instantiate tc '[ VDType t ] = tc t

  Instantiate tc '[ VDevice t i ] = tc '(t, i)
  
  Instantiate tc '[ VNat t ] = tc t

  Instantiate tc '[ VNatList t ] = tc t

  Instantiate tc '[ VNatTupleList t ] = tc t

  Instantiate tc '[ VType t ] = tc t

  Instantiate tc (( VDType t) ': rest) = Instantiate (tc t) rest

  Instantiate tc (( VDevice t i) ': rest) = Instantiate (tc '(t, i) ) rest

  Instantiate tc ((VNat t) ': rest) = Instantiate (tc t) rest

  Instantiate tc ((VNatList t) ': rest) = Instantiate (tc t) rest

  Instantiate tc ((VNatTupleList t) ': rest) = Instantiate (tc t) rest

  Instantiate tc ((VType t) ': rest) = Instantiate (tc t) rest


type family MapPaths conf plist where
  MapPaths conf '[] = '[]
  MapPaths conf (p ': rest) = (MatchR p conf VNil) ': (MapPaths conf rest)

type family InstSpec' conf (arch :: Value) :: Type where
  InstSpec' conf (VArch tp spec plist) = Instantiate spec (MapPaths conf plist)

type family InstSpec conf tcn where
  InstSpec conf tcn = InstSpec' conf (MatchR tcn conf VNil)

type family InstType' conf (arch :: Value) :: Type where
  InstType' conf (VArch tp spec plist) = Instantiate tp (MapPaths conf plist)

type family InstType conf tcn where
  InstType conf tcn = InstType' conf (MatchR tcn conf VNil)


-- create encoder types
type family MkEncodersImpl
  ( conf :: Configuration )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [ Type ] where

  MkEncodersImpl conf _ '[] = '[]
  MkEncodersImpl conf e ( '( s, Image ) ': rest ) = Tagged '( e, s ) (InstType conf '[ PSym "ImageEncoder", PSym e, PSym s ] ) ': ( MkEncodersImpl conf e rest )
  MkEncodersImpl conf e ( '( s, Scalar ) ': rest ) = Tagged '( e, s ) (InstType conf '[ PSym "ScalarEncoder", PSym e, PSym s ] ) ': ( MkEncodersImpl conf e rest )
  MkEncodersImpl conf e ( '( s, Text ) ': rest ) = Tagged '( e, s ) (InstType conf '[ PSym "TextEncoder", PSym e, PSym s ] ) ': ( MkEncodersImpl conf e rest )
  MkEncodersImpl conf e ( '( s, Categorical ) ': rest ) = Tagged '( e, s ) (InstType conf '[ PSym "CategoricalEncoder", PSym e, PSym s ] ) ': ( MkEncodersImpl conf e rest )
  MkEncodersImpl conf e ( _ ': rest ) = MkEncodersImpl conf e rest

data MkEncoders :: Configuration -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkEncoders conf) '( s, xs ) = MkEncodersImpl conf s xs


-- create encoder spec types
type family MkEncoderSpecsImpl
  ( conf :: Configuration )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [ Type ] where

  MkEncoderSpecsImpl _ _ '[] = '[]
  MkEncoderSpecsImpl conf e ( '( s, Image ) ': rest ) = Tagged '( e, s ) (Proxy (InstSpec conf '[ PSym "ImageEncoder", PSym e, PSym s ] ) ) ': ( MkEncoderSpecsImpl conf e rest )
  MkEncoderSpecsImpl conf e ( '( s, Scalar ) ': rest ) = Tagged '( e, s ) (Proxy (InstSpec conf '[ PSym "ScalarEncoder", PSym e, PSym s ] ) ) ': ( MkEncoderSpecsImpl conf e rest )
  MkEncoderSpecsImpl conf e ( '( s, Text ) ': rest ) = Tagged '( e, s ) (Proxy (InstSpec conf '[ PSym "TextEncoder", PSym e, PSym s ] ) ) ': ( MkEncoderSpecsImpl conf e rest )
  MkEncoderSpecsImpl conf e ( '( s, Categorical ) ': rest ) = Tagged '( e, s ) (Proxy (InstSpec conf '[ PSym "CategoricalEncoder", PSym e, PSym s ] ) ) ': ( MkEncoderSpecsImpl conf e rest )
  MkEncoderSpecsImpl conf e ( _ ': rest ) = MkEncoderSpecsImpl conf e rest

data MkEncoderSpecs :: Configuration -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkEncoderSpecs conf) '( s, xs ) = MkEncoderSpecsImpl conf s xs


-- create summarizer types
type family MkSummarizersImpl
  ( conf :: Configuration )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [ Type ] where

  MkSummarizersImpl _ _ '[] = '[]    
  MkSummarizersImpl conf e ( '( s, Related a ) ': rest ) = Tagged '( e, s ) (InstType conf '[ PSym "Summarizer" ] ) ': ( MkSummarizersImpl conf e rest )
  MkSummarizersImpl conf e ( _ ': rest ) = MkSummarizersImpl conf e rest

data MkSummarizers :: Configuration -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkSummarizers conf) '( s, xs ) = MkSummarizersImpl conf s xs


-- create summarizer spec types
type family MkSummarizerSpecsImpl
  ( conf :: Configuration )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [ Type ] where

  MkSummarizerSpecsImpl _ _ '[] = '[]
  MkSummarizerSpecsImpl conf e ( '( s, Related a ) ': rest ) = Tagged '( e, s ) (Proxy (InstSpec conf '[ PSym "Summarizer" ] ) ) ': ( MkSummarizerSpecsImpl conf e rest )  
  MkSummarizerSpecsImpl conf e ( _ ': rest ) = MkSummarizerSpecsImpl conf e rest

data MkSummarizerSpecs :: Configuration -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkSummarizerSpecs conf) '( s, xs ) = MkSummarizerSpecsImpl conf s xs


-- create autoencoder types
type family MkAutoEncodersImpl
  ( depth :: Nat )
  ( dtype :: DType )
  ( device :: ( DeviceType, Nat ) )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [Type] where
  MkAutoEncodersImpl 0 dtype device entity ps = '[ Tagged '( entity, 0 ) (AutoEncoder (SizeSum ps) EncoderShape BottleneckSize DecoderShape dtype device ) ]

  MkAutoEncodersImpl d dtype device entity ps = (Tagged '( entity, d ) (AutoEncoder (SizeSum ps) EncoderShape BottleneckSize DecoderShape dtype device ) ) ': (MkAutoEncodersImpl (d - 1) dtype device entity ps)

data MkAutoEncoders :: DType -> (DeviceType, Nat) -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkAutoEncoders dtype device) '( s, xs ) = MkAutoEncodersImpl 0 dtype device s xs


-- create autoencoder spec types
type family MkAutoEncoderSpecsImpl
  ( depth :: Nat )
  ( dtype :: DType )
  ( device :: ( DeviceType, Nat ) )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [Type] where
  MkAutoEncoderSpecsImpl 0 dtype device entity ps = '[ Tagged '( entity, 0 ) (Proxy (AutoEncoderSpec (SizeSum ps) EncoderShape BottleneckSize DecoderShape dtype device ) ) ]

  MkAutoEncoderSpecsImpl d dtype device entity ps = (Tagged '( entity, d ) (Proxy (AutoEncoderSpec (SizeSum ps) EncoderShape BottleneckSize DecoderShape dtype device ) ) ) ': (MkAutoEncoderSpecsImpl (d - 1) dtype device entity ps)

data MkAutoEncoderSpecs :: DType -> (DeviceType, Nat) -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkAutoEncoderSpecs dtype device) '( s, xs ) = MkAutoEncoderSpecsImpl 0 dtype device s xs


-- create decoder types
type family MkDecodersImpl
  ( conf :: Configuration )
  ( dtype :: DType )
  ( device :: ( DeviceType, Nat ) )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [ Type ] where

  MkDecodersImpl _ _ _ _ '[] = '[]

  MkDecodersImpl conf dtype device e ( '( s, Image ) ': rest ) = (Tagged '( e, s ) ( CNN ( conf ^*# '[ PSym "ImageChannels" ] ) ( conf ^*## '[ PSym "ImageKernels" ] ) dtype device) ) ': ( MkDecodersImpl conf dtype device e rest )

  MkDecodersImpl conf dtype device e ( '( s, Scalar ) ': rest ) = (Tagged '( e, s ) (MLP '[1, 10, 10, ScalarEmbeddingSize] dtype device ) ) ': ( MkDecodersImpl conf dtype device e rest )

  MkDecodersImpl conf dtype device e ( '( s, Text ) ': rest ) = (Tagged '( e, s ) ( RNN TokenEmbeddingSize TextEmbeddingSize dtype device ) ) ': ( MkDecodersImpl conf dtype device e rest )

  MkDecodersImpl conf dtype device e ( '( s, Categorical ) ': rest ) = (Tagged '( e, s ) ( Embedding (conf ^# '[PSym "CategoricalEmbeddingCount"]) CategoricalEmbeddingSize dtype device) ) ': ( MkDecodersImpl conf dtype device e rest )
  
  --MkDecodersImpl conf dtype device e ( '( s, Categorical ) ': rest ) = (Tagged '( e, s ) ( Embedding (Just 0) ((^#) (MatchR '[PSym "CategoricalEmbeddingCount"] conf (VNat 2))) CategoricalEmbeddingSize dtype device) ) ': ( MkDecodersImpl conf dtype device e rest )  

  MkDecodersImpl conf dtype device e ( _ ': rest ) = MkDecodersImpl conf dtype device e rest

data MkDecoders :: Configuration -> DType -> (DeviceType, Nat) -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkDecoders conf dtype device) '( s, xs ) = MkDecodersImpl conf dtype device s xs


-- create decoder spec types
type family MkDecoderSpecsImpl
  ( conf :: Configuration )
  ( dtype :: DType )
  ( device :: ( DeviceType, Nat ) )
  ( entity :: Symbol )
  ( properties :: [ ( Symbol, Type ) ] )
  :: [ Type ] where

  MkDecoderSpecsImpl _ _ _ _ '[] = '[]

  MkDecoderSpecsImpl conf dtype device e ( '( s, Image ) ': rest ) = ( Tagged '( e, s ) ( Proxy ( CNNSpec ( conf ^*# '[ PSym "ImageChannels" ] ) ( conf ^*## '[ PSym "ImageKernels" ] ) dtype device ) ) ) ': ( MkDecoderSpecsImpl conf dtype device e rest )

  MkDecoderSpecsImpl conf dtype device e ( '( s, Scalar ) ': rest ) = ( Tagged '( e, s )  ( Proxy (MLPSpec '[1, 10, 10, ScalarEmbeddingSize] dtype device ) ) ) ': ( MkDecoderSpecsImpl conf dtype device e rest )  

  MkDecoderSpecsImpl conf dtype device e ( '( s, Text ) ': rest ) = ( Tagged '( e, s) ( Proxy (RNNSpec TokenEmbeddingSize TextEmbeddingSize dtype device ) ) ) ': ( MkDecoderSpecsImpl conf dtype device e rest )

  MkDecoderSpecsImpl conf dtype device e ( '( s, Categorical ) ': rest ) = ( Tagged '( e, s ) ( Proxy ( EmbeddingSpec (conf ^# '[PSym "CategoricalEmbeddingCount"]) CategoricalEmbeddingSize dtype device ) ) ) ': ( MkDecoderSpecsImpl conf dtype device e rest )

  --MkDecoderSpecsImpl conf dtype device e ( '( s, Categorical ) ': rest ) = ( Tagged '( e, s ) ( Proxy ( EmbeddingSpec (Just 0) ((^#) (MatchR '[PSym "CategoricalEmbeddingCount"] conf (VNat 2))) CategoricalEmbeddingSize dtype device ) ) ) ': ( MkDecoderSpecsImpl conf dtype device e rest )  

  MkDecoderSpecsImpl conf dtype device e ( _ ': rest ) = MkDecoderSpecsImpl conf dtype device e rest

data MkDecoderSpecs :: Configuration -> DType -> (DeviceType, Nat) -> (TyFun (Symbol, [(Symbol, Type)]) [Type]) -> Type

type instance Apply (MkDecoderSpecs conf dtype device) '( s, xs ) = MkDecoderSpecsImpl conf dtype device s xs


instance
  ( StandardFloatingPointDTypeValidation device dtype
  , BasicArithmeticDTypeIsValid device dtype
  , ComparisonDTypeIsValid device dtype
  , KnownDType dtype
  , KnownDevice device
  , KnownNat depth
  , RandDTypeIsValid device dtype
  , inp ~ DomainInputR entityTypes dtype device
  , out ~ inp
  ) =>
  HasForward (StarCoder conf entityTypes depth dtype device) (HList inp) (HList out) where
  forwardStoch model@(StarCoder {..}) inp = return (forward model inp)
  forward model inp = reconstruct model True (starcoder model True (fromIntegral depth') inp)
    where
      depth' = natVal (Proxy :: Proxy depth)

{-
At given depth > 0, needs: representations and bottlenecks from previous set of autoencoders
At depth = 0, needs: inputs
-}

starcoder ::
  forall
  conf
  entityTypes
  depth
  input
  dtype
  device .
  ( input ~ DomainInputR entityTypes dtype device
  ) => StarCoder conf entityTypes depth dtype device -> Bool -> Int -> (HList input) -> (HList input)
starcoder model train 0 input = input

starcoder model train d input = starcoder model train (d - 1) input






reconstruct ::
  forall
  conf
  entityTypes
  depth
  input
  repSize
  dtype
  device .
  ( input ~ DomainInputR entityTypes dtype device
  , repSize ~ Last DecoderShape
  ) => StarCoder conf entityTypes depth dtype device -> Bool -> (HList input) -> (HList input)
reconstruct model train reps = reps

  
