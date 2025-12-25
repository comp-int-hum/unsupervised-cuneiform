module UnsupervisedCuneiform.Embedding ( Embedding(..)
                                       , EmbeddingSpec(..)
                                       ) where

import System.IO.Unsafe
import GHC.TypeLits
import GHC.Types
import GHC.Generics
import Data.Proxy
import Torch.Typed hiding (FoldLayers, EmbeddingSpec, Embedding)
import qualified Torch.Typed as T


data EmbeddingSpec (numEmbeds :: Nat) (embedSize :: Nat) (dtype :: DType) (device :: (DeviceType, Nat)) = EmbeddingSpec
  deriving (Eq, Show)


data Embedding (numEmbeds :: Nat) (embedSize :: Nat) (dtype :: DType) (device :: (DeviceType, Nat)) where
  Embedding ::
    { emb :: T.Embedding (Just 0) numEmbeds embedSize Learned dtype device
    } -> Embedding numEmbeds embedSize dtype device
  deriving (Generic)


instance
  ( KnownDType dtype
  , KnownDevice device
  , RandDTypeIsValid device dtype
  , Randomizable (T.EmbeddingSpec (Just 0) numEmbeds embedSize Learned dtype device) (T.Embedding (Just 0) numEmbeds embedSize Learned dtype device)
  ) =>
  Randomizable (Proxy (EmbeddingSpec numEmbeds embedSize dtype device)) (Embedding numEmbeds embedSize dtype device) where
  sample _ = Embedding <$> sample (LearnedEmbeddingWithRandomInitSpec :: T.EmbeddingSpec (Just 0) numEmbeds embedSize Learned dtype device)


instance Show (Embedding numEmbeds embedSize dtype device) where
  show _ = "Embedding"

instance Parameterized (Embedding numEmbeds embedSize dtype device)
  
