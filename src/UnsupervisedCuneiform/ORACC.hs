module UnsupervisedCuneiform.ORACC (parseORACC) where

import Control.Monad (liftM)
import qualified Data.Map as Map
import qualified Codec.Archive.Zip as Z
import Data.ByteString (ByteString)
import Data.Aeson (decodeStrict, FromJSON(..), (.:), withObject)
import Data.Maybe (catMaybes, fromMaybe)
import UnsupervisedCuneiform.Artifact (Artifact(..))


newtype ORACC = ORACC { unoracc :: Artifact
                      }
                
instance FromJSON ORACC where
  parseJSON x = ORACC <$> parseJSON' x
    where
      parseJSON' = withObject "Artifact" $ \v -> Artifact
        <$> v .: "textid"
        <*> return Nothing
        <*> return Nothing
        <*> return Nothing
        <*> return []
        <*> return []
        <*> return []
        <*> ((sequence . map parseJSON) (fromMaybe [] $ Just [])) --(v .: "cdl" :: [Value]))

-- catalogue, *portal, corpus, metadata, sortcodes, index*, gloss*, P*
-- corpusjson: textid, cdl
parseORACC :: String -> IO (Either String [Artifact])
parseORACC t = Z.withArchive t $ do
  et <- Z.getEntries
  --(return . Right) [def]
  let ents = Map.keys et
  vs <- sequence $ map (liftM parseArtifact . Z.getEntry) ents
  return . Right . catMaybes $ vs --concat . map readArtifacts . catMaybes $ vs --[def | _ <- bs]

parseArtifact :: ByteString -> Maybe Artifact
parseArtifact bs = unoracc <$> o
  where
    o = decodeStrict bs :: Maybe ORACC
