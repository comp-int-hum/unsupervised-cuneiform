module Main (main) where

import Prelude hiding (readFile, writeFile)
import Data.Either (rights)
import Control.Monad (liftM)
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Data.ByteString.Lazy (readFile, toStrict, fromStrict)
import qualified Data.ByteString as BS
import qualified Codec.Compression.GZip as GZip
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import qualified UnsupervisedCuneiform as UC
import UnsupervisedCuneiform.Artifact (mergeArtifacts, toJsonL)
import UnsupervisedCuneiform.Image (hydrate)

data Args w = Args { fields :: w ::: Text <?> ""
                   , atf :: w ::: Text <?> ""
                   , oraccPath :: w ::: Text <?> ""
                   , imagePath :: w ::: Text <?> ""
                   , output :: w ::: Maybe Text <?> ""
                   }
  deriving (Generic)

instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)


intersectionsWith :: (Ord k) => (v -> v -> v) -> [Map k v] -> Map k v
intersectionsWith _ [] = Map.empty
intersectionsWith _ (x:[]) = x
intersectionsWith f (x:xs) = Map.intersectionWith f x (intersectionsWith f xs)


main :: IO ()
main = do
  ps <- unwrapRecord "" :: IO (Args Unwrapped)

  Right csve <- (liftM (UC.parseCSV . decodeUtf8Lenient . toStrict . GZip.decompress) . readFile . Text.unpack . fields) ps
  Right atfe <- (liftM (UC.parseATF . decodeUtf8Lenient . toStrict . GZip.decompress) . readFile . Text.unpack . atf) ps
  os <- (listDirectory . Text.unpack . oraccPath) ps
  is <- (listDirectory . Text.unpack . imagePath) ps

  oracc <- (liftM (concat . rights) . sequence) [UC.parseORACC $ ((Text.unpack . oraccPath) ps) </> o | o <- take 5 os, ".zip" `isSuffixOf` o]

  images <- (liftM (concat . rights) . sequence) [UC.parseImage $ ((Text.unpack . imagePath) ps) </> i | i <- is, ".jpg" `isSuffixOf` i]

  let csvA = Map.fromList [(c, c) | c <- csve]
      atfA = Map.fromList [(c, c) | c <- atfe]
      oraccA = Map.fromList [(c, c) | c <- oracc]
      imagesA = Map.fromList [(c, c) | c <- images]
      comb = Map.elems $ intersectionsWith mergeArtifacts [csvA, atfA, imagesA, oraccA]

  final <- sequence $ map (hydrate ((Text.unpack . imagePath) ps)) comb
  case output ps of Just f -> BS.writeFile (Text.unpack f) ((toStrict . GZip.compress . fromStrict . toJsonL) (take 3 final))
                    _ -> return ()
