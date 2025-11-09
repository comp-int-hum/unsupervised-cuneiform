module Main where

import Prelude hiding (readFile, writeFile)
import Control.Monad (liftM)
import Effectful.Reader.Dynamic
import qualified Data.ByteString as BS
import qualified Codec.Compression.GZip as GZip
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Web.Hyperbole (run, liveApp, quickStartDocument, runPage)
import UnsupervisedCuneiform.Artifact (fromJsonL)
import UnsupervisedCuneiform.Server (app) --, runArtifactsSession)

data Args w = Args { corpus :: w ::: String <?> ""
                   , port :: w ::: Int <?> ""
                   }
  deriving (Generic)

instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)


main :: IO ()
main = do
  ps <- unwrapRecord "" :: IO (Args Unwrapped)  
  xs <- (liftM (fromJsonL . BS.toStrict . GZip.decompress . BS.fromStrict) . BS.readFile) (corpus ps)
  --print $ length c
  --(Text.unpack f) ((toStrict . GZip.compress . fromStrict . toJsonL) final)
  --return ()
  run (port ps) $ app xs
  --liveApp quickStartDocument (runReader xs $ runPage page)
