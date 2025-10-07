module UnsupervisedCuneiform.ORACC (parseORACC) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Codec.Archive.Zip as Z
import Data.Aeson

-- catalogue, *portal, corpus, metadata, sortcodes, index*, gloss*, P*
parseORACC :: String -> IO (Either String ())
parseORACC t = Z.withArchive t $ do
  et <- Z.getEntries
  --liftIO $ print (Map.keys et)
  return $ Left ""
