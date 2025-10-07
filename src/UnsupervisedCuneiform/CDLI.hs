module UnsupervisedCuneiform.CDLI (parseATF, parseCSV) where

import Control.Monad.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Text.Megaparsec (token, runParser, runParserT', runParser', State(..), PosState(..), Parsec(..), sepEndBy1, ParsecT(..), some)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.State (initialState)
import Data.Void (Void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Csv (decode, HasHeader(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace (traceShowId)

type Parser a = ParsecT Void String Identity a

cdliId :: Parser String
cdliId = string "&P" -- >>= \x -> return $ Text.pack x
--recordSep = eol >> eol

--start = token '&'
spec :: Parser [String]
spec = char '&' >>= \x -> return [] -- >> return (Text.pack "error") --sepEndBy1 cdliId eol

parseATF :: Text -> Either String [String]
parseATF t = case res of Left l -> Left "error" -- $ (show l :: String)
                         Right r -> Right r
  where
    state = initialState "" (Text.unpack t)
    res = snd $ runParser' spec state

parseCSV :: Text -> Either String [Map ByteString ByteString]
parseCSV t = do
  v <- decode NoHeader ((fromStrict . encodeUtf8) t)
  let h = Vector.head v
  return [Map.fromList $ zip h r | r <- (Vector.toList . Vector.tail) v]
