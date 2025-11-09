module UnsupervisedCuneiform.Image ( parseImage
                                   , hydrate
                                   ) where

import Prelude hiding (readFile)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Data.Either (fromRight)
import Data.List ((!?), singleton)
--import Data.ByteString.Lazy (ByteString, toStrict) --, readFile)
import Data.ByteString (readFile)
import System.Directory (doesFileExist)
--import Data.Vector hiding ((!), (!?), (++), singleton, head, length, map)
import Control.Lens ((&), (.~), (?~), (^.))
import Text.Regex.TDFA ((=~))
import Codec.Picture.Jpg (decodeJpegWithMetadata)
--import Codec.Picture.Types (DynamicImage) --, Image(..), dynamicMap, dynamicPixelMap)
--import Codec.Picture.Metadata (Metadatas)
import UnsupervisedCuneiform.Artifact (Artifact, def, drawings, photos, identifier)
import Torch.Vision (fromDynImage)
import Torch.Tensor (asValue, toType)
import qualified Torch.DType as DType --(DType(..))

filePattern :: String
filePattern = ".*/(P[[:digit:]]+)_([a-z]+)\\.jpg"


parseImage :: String -> IO (Either String [Artifact])
parseImage fn = do
  let (_, _, _, n) = (fn =~ filePattern) :: (String, String, String, [String])
      r = case n !? 1 of Just "photo" -> [def & photos .~ [] & identifier ?~ (n !! 0)]
                         Just "line" -> [def & drawings .~ [] & identifier ?~ (n !! 0)]
                         _ -> []
  return $ Right r

hydrate :: String -> Artifact -> IO Artifact
hydrate path a = do
  let i = fromMaybe "" (a ^. identifier)
      pPath = (path ++ "/" ++ i ++ "_photo.jpg")
      lPath = (path ++ "/" ++ i ++ "_line.jpg")
  pe <- doesFileExist pPath
  le <- doesFileExist lPath
  -- use Data.List.NonEmpty
  p <- if pe then (liftM (singleton . fromDynImage . fst . fromRight undefined . decodeJpegWithMetadata) . readFile) pPath else return []
  l <- if le then (liftM (singleton . fromDynImage . fst . fromRight undefined . decodeJpegWithMetadata) . readFile) lPath else return []
  let pv = map (head . asValue . toType DType.Float) p
      lv = map (head . asValue . toType DType.Float) l
  return $ a & photos .~ pv & drawings .~ lv
