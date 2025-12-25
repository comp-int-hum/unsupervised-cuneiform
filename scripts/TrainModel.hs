module Main (main) where
import Data.Tagged
import GHC.Types
import Control.DeepSeq
import Torch.Typed (DeviceType(..), DType(..), sample, forward, mseLoss, hzip, hmap, Reduction(..), Tensor(..), hzip', hmap', HList(..), MakeIndependent, ToDependent, Apply'(..), Parameter, HMapM', KnownDevice, HList(..), pattern (:.))
import Torch.Typed hiding (EmbeddingSpec, Embedding, RNN, RNNSpec)
import UnsupervisedCuneiform.Trie --Configuration -- (readConfig, Config(..))
import UnsupervisedCuneiform.StarCoder (StarCoder, StarCoderSpec(..), SampleInputF(..), DomainInputR, Configuration(..), InstType(..), InstSpec(..))
import UnsupervisedCuneiform.Domain (Cuneiform)
import UnsupervisedCuneiform.MLP (MLP(..), MLPSpec(..))
import UnsupervisedCuneiform.Embedding (Embedding(..), EmbeddingSpec(..))
import UnsupervisedCuneiform.CNN (CNN(..), CNNSpec(..))
import UnsupervisedCuneiform.RNN (RNN(..), RNNSpec(..))

-- type EncoderShape = '[512, 256]
-- type DecoderShape = '[256, 512]
-- type TextEmbeddingSize = 2 --128
-- type TokenEmbeddingSize = 2 --32
-- type ScalarEmbeddingSize = 2 --5
-- type CategoricalEmbeddingSize = 2 --64
-- type HiddenSize = 3 --64
-- type Depth = 3
-- type EmbeddingCount = 4 --100
-- type BatchSize = 1 --50
-- type ImageWidth = 1024
-- type ImageHeight = 768
-- type ImageChannels = '[3, 10, 10, 10, 10, 10, 10, 1]
-- type ImageKernels = '[ '( 3, 3)
--                      , '( 3, 3)
--                      , '( 3, 3)
--                      , '( 3, 3)
--                      , '( 3, 3)
--                      , '( 3, 3)
--                      , '( 3, 3)                     
--                      ]
-- type ImageEmbeddingSize = 48
-- type EntityCount = 5


type Config = MkTrieR '[ '( '[ PSym "EntityCount" ], VNat 5 )
                       , '( '[ PSym "BatchSize" ], VNat 1 )
                       , '( '[ PSym "MaxSequenceLength" ], VNat 16 )
                       , '( '[ PSym "Depth" ], VNat 3 )
                       , '( '[ PSym "EncoderShape" ], VNatList [512, 256] )
                       , '( '[ PSym "DecoderShape" ], VNatList [256, 512] )                                                                     
                       , '( '[ PSym "BottleneckSize" ], VNat 8 )
                       , '( '[ PSym "TextEmbeddingSize" ], VNat 3 )
                       , '( '[ PSym "TextHiddenSize" ], VNat 2 )                       
                       , '( '[ PSym "TokenEmbeddingSize" ], VNat 2 )
                       , '( '[ PSym "CategoricalEmbeddingSize" ], VNat 2 )
                       , '( '[ PSym "CategoricalEmbeddingCount" ], VNat 13 )
                       , '( '[ PSym "CategoricalEmbeddingPad" ], VNat 0 )                       
                       , '( '[ PSym "ScalarEmbeddingShape" ], VNatList [1, 10, 10] )
                       , '( '[ PSym "HiddenSize" ], VNat 2 )
                       , '( '[ PSym "ImageWidth" ], VNat 1024 )                                              
                       , '( '[ PSym "ImageHeight" ], VNat 768 )
                       , '( '[ PSym "ImageEmbeddingSize" ], VNat 48 )
                       , '( '[ PSym "ImageKernels" ], VNatTupleList [ '( 3, 3)
                                                                    , '( 3, 3)
                                                                    , '( 3, 3)
                                                                    , '( 3, 3)
                                                                    , '( 3, 3)
                                                                    , '( 3, 3)
                                                                    , '( 3, 3)                     
                                                                    ]
                          )
                       , '( '[ PSym "ImageChannels" ], VNatList '[3, 10, 10, 10, 10, 10, 10, 1] )
                       , '( '[ PSym "Device" ], VDevice 'CPU 0 )
                       , '( '[ PSym "DType" ], VDType 'Float )
                       , '( '[ PSym "Summarizer" ], VArch RNN RNNSpec '[ '[ PSym "BottleneckSize" ], '[ PSym "BottleneckSize" ], '[ PSym "DType" ], '[ PSym "Device" ] ] )                       
                       , '( '[ PSym "ScalarEncoder" ], VArch MLP MLPSpec '[ '[ PSym "ScalarEmbeddingShape" ], '[ PSym "DType" ], '[ PSym "Device" ] ] )
                       , '( '[ PSym "CategoricalEncoder" ], VArch Embedding EmbeddingSpec '[ '[ PSym "CategoricalEmbeddingCount" ], '[ PSym "CategoricalEmbeddingSize" ], '[ PSym "DType" ], '[ PSym "Device" ] ] )
                       , '( '[ PSym "TextEncoder" ], VArch RNN RNNSpec '[ '[ PSym "TextEmbeddingSize" ], '[ PSym "TextHiddenSize" ], '[ PSym "DType" ], '[ PSym "Device" ] ] )
                       , '( '[ PSym "ImageEncoder" ], VArch CNN CNNSpec '[ '[ PSym "ImageChannels" ], '[ PSym "ImageKernels" ], '[ PSym "DType" ], '[ PSym "Device" ] ] )
                       
                       ]


class ToMSE (xs :: [Type]) where
  toMSE :: HList xs -> HList xs -> Tensor '( 'CPU, 0 ) 'Float '[]

instance
  ( tp ~ Tagged t (Tensor device dtype shape)
  , KnownDevice device
  , device ~ '( 'CPU, 0 )
  --, HasToDType dtype 'Float
  ) =>
  ToMSE '[tp] where
  toMSE (a :. HNil) (b :. HNil) = mseLoss @ReduceMean ((toDType @'Float @dtype . untag) a) ((toDType @'Float @dtype . untag) b)

instance
  ( ToMSE (y ': xs)
  , tp ~ Tagged t (Tensor device dtype shape)
  , KnownDevice device
  , device ~ '( 'CPU, 0 )
  --, HasToDType dtype 'Float
  ) =>
  ToMSE (tp ': y ': xs) where
  toMSE (a :. xs) (b :. ys) = (mseLoss @ReduceMean ((toDType @'Float @dtype . untag) a) ((toDType @'Float @dtype . untag) b)) + (toMSE xs ys)


instance
  Apply'
    ToDependent
    (Tensor device dtype shape)
    (IO (Parameter device dtype shape))
  where
  apply' _ = (return . undefined)



type CuneiformConfiguration = '[ '( "BottleneckShape", Left '[ 128 ] )
                               , '( "EncoderShape", Left '[ 512, 256 ] )
                               , '( "DecoderShape", Left '[ 256, 512 ] )
                               , '( "TextEmbeddingShape", Left '[ 256, 512 ] )
                               , '( "CategoricalEmbeddingShape", Left '[ 10 ] )
                               ]

sp = StarCoderSpec :: StarCoderSpec Config Cuneiform 3 'Float '( 'CPU, 0 )

--sp' = sample sp

main :: IO ()
main = do
  --ps <- readConfig
  --let ms = read (mlpSize ps) :: [Int]
  
  --let sp = StarCoderSpec :: StarCoderSpec Config Cuneiform 3 'Float '( 'CPU, 0 )
  model <- sample sp :: IO (StarCoder Config Cuneiform 3 'Float '( 'CPU, 0 ))
  inp <- sampleInputF @(DomainInputR Cuneiform 'Float '( 'CPU, 0 ) ) -- @'[]
  

  
  let lr = 0.1
      params = flattenParameters model
      opt = mkAdam 0 0.9 0.999 params      
      out = forward model inp
      
      --loss = hmap (\(a, b) -> mseLoss @ReduceMean (untag a) (untag b) :: Tensor '( 'CPU, 0 ) 'Float '[]) (hzip inp out)
      --loss = hmap' (\(a, b) -> 1.0 :: Float)
      loss = toMSE inp out --(hzip inp out)
      --grads = grad loss params
      --tensors = hmap' ToDependent params
      --(tensors', optim') = step lr grads tensors opt
  --parameters <- tensors' `deepseq` (return 10)
  --parameters' <- (return . hmap' ToDependent) tensors'
  --parameters' <- toindep tensors' --hmapM MakeIndependent tensors'

  --(model', opt') <- runStep' model opt lr grads
      --loss = hmap 
      --loss = sum [mseLoss (untag a) (untag b) | (a, b) <- hzip inp out]      
  print loss --out --loss -- loss
  --print inp
  
toindep ::
  forall parameters tensors .
  ( --parameters ~ tensors
  HMapM' IO MakeIndependent tensors parameters
  ) =>
  (HList tensors) ->
  IO (HList parameters)
toindep tens = hmapM' MakeIndependent tens
