module UnsupervisedCuneiform.Config ( Config(..)
                                    , readConfig
                                    --, Unwrapped
                                    --, unwrapRecord
                                    ) where


--import Data.Default
import Data.Text (Text)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..), type (<!>)(..))


type DefaultDevice = '("CPU", 0)
type DefaultSummaryShape = 128
type DefaultRNNSHape = '()
type DefaultCategoricalEmbeddingShape = 32
type DefaultCNNShape = '(4, 4)
type DefaultCNNStack = '[ '(3, 10, 4, 4), '(10, 10, 4, 4), '(10, 10, 4, 4) ]
type DefaultImageEmbeddingShape = 32




data Config w = Config { depth :: w ::: Int <!> "3"
                       , mlpSize :: w ::: String <!> "[10, 20]"
                       , categoryEmbeddingSize :: w ::: Int <!> "64"
                       , textHiddenSize :: w ::: Int <!> "32"
                       , bottleneckSize :: w ::: Int <!> "128"
  --fields :: w ::: Text <?> ""
                       -- , atf :: w ::: Text <?> ""
                       -- , oraccPath :: w ::: Text <?> ""
                       -- , imagePath :: w ::: Text <?> ""
                       -- , output :: w ::: Maybe Text <?> ""
                       -- , 
                       }
  deriving (Generic)

instance ParseRecord (Config Wrapped)
deriving instance Show (Config Unwrapped)

readConfig :: IO (Config Unwrapped)
readConfig = unwrapRecord ""

--data Config = Config { depth :: Int
--                     }
