module UnsupervisedCuneiform.CDLI (parseATF, parseCSV) where

import Prelude hiding (lines)
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Applicative hiding (some, many)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Text.Megaparsec (token, runParser, runParserT', runParser', State(..), PosState(..), Parsec, sepEndBy1, ParsecT, some, many, someTill_, (<|>), takeWhile1P, anySingle, anySingleBut, takeRest, someTill, manyTill, runParserT, choice, try, lookAhead, eof, getParserState, sepEndBy, notFollowedBy, withRecovery, skipManyTill, anySingle)
import Text.Megaparsec.Char (char, eol, string, hspace1, space1, digitChar, punctuationChar, space)
import Text.Megaparsec.State (initialState)
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Lens ((.~), (?~), (&), (^.))
import Data.Void (Void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Csv (decode, HasHeader(..), decodeByName)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Default (Default(..))
import Debug.Trace (traceShowId)
import Text.Regex.TDFA ((=~))
import Data.Csv (FromNamedRecord(..))
import qualified Data.Csv as C
import UnsupervisedCuneiform.Artifact (Artifact(..), def, identifier, designation, language, oraccContent, atfContent, objectType)

type Parser = ParsecT Void String Identity

idPattern :: String
idPattern = ".*CDLI Lexical ([[:digit:]]+).*"

remainder :: Parser String
remainder = manyTill (anySingleBut '\n') eol

parseATF :: Text -> Either String [Artifact]
parseATF t = Right res
  where
    ts = ["&P" ++ (Text.unpack x) | x <- Text.splitOn "&P" t, x /= ""]
    res = rights [runIdentity $ runParserT artifact "" t' | t' <- ts]

parseCSV :: Text -> Either String [Artifact]
parseCSV t = do
  let t' = (fromStrict . encodeUtf8) t
      Right (_, rs) = decodeByName t'
  Right $ Vector.toList rs

artifacts :: Parser [Artifact]
artifacts = do
  xs <- (withRecovery (\e -> do
                          _ <- skipManyTill anySingle (lookAhead $ char '&')
                          return def
                      ) artifact) `sepEndBy1` space
  s <- getParserState
  eof
  return xs

artifact :: Parser Artifact
artifact = do
  (i, d) <- header
  -- _ <- many $ version
  -- _ <- many comment
  l <- languageCode
  --_ <- many comment
  -- _ <- many $ dummy  
  tps <- many $ object
  ls <- optional lines
  cs <- many column
  fs <- many surface
  let tp = if length tps > 0 then tps !! 0 else "@unknown"

  return $ def & language ?~ l & identifier ?~ i & designation ?~ d & objectType ?~ tp & atfContent .~ (concat $ map (\(a, b, c) -> [a] ++ b ++ c) fs)

header :: Parser (String, String)
header = do
  _ <- string "&P"
  i <- some digitChar
  hspace1
  char '='
  hspace1
  d <- remainder
  return ('P':i, d)


languageCode :: Parser String
languageCode = do
  string "#atf: lang "
  remainder

object :: Parser String
object = do
  tp <- choice $ map (string) ["@tablet", "@bulla", "@envelope", "@object"]
  r <- remainder
  _ <- many breakage
  return $ if tp == "@object" then r else tp


surface :: Parser (String, [String], [String])
surface = do
  f <- choice $ map (string) ["@obverse", "@reverse", "@surface", "@fragment"]
  --let f = fromMaybe "@unknown" f'
  _ <- remainder
  ncl <- optional lines
  --cs <- many edge
  cs <- many $ choice [column, edge, seal, fragment] --many column -- (column <|> edge)
  return (f, fromMaybe [] ncl, concat $ map (\(a, b) -> a:b) cs)

column :: Parser (String, [String])
column = do
  string "@column"
  space1
  --n <- some digitChar
  n <- remainder
  ls <- optional lines
  return (n, fromMaybe [] ls)

seal :: Parser (String, [String])
seal = do
  string "@seal"
  space1
  n <- some digitChar
  _ <- remainder
  ls <- lines
  return (n, ls)

fragment :: Parser (String, [String])
fragment = do
  string "@fragment"
  space1
  n <- some digitChar
  _ <- remainder
  ls <- lines
  return (n, ls)

edge :: Parser (String, [String])
edge = do
  --n <- try $ choice $ map (try . string) ["@left", "@right", "@top", "@bottom", "@column"]
  n <- choice $ map string ["@top", "@bottom", "@right", "@left"]
  _ <- remainder
  ls <- many line
  return (n, ls)

line :: Parser String
line = do
  f <- choice $ map try [comment, breakage, broken, unbroken, arrow, mistake]
  return f

lines :: Parser [String]
lines = try $ some line

-- line-types

comment :: Parser String
comment = do
  char '#'
  --notFollowedBy $ string $ "atf: lang"
  _ <- remainder
  return "comment"
  
breakage :: Parser String
breakage = do
  char '$'
  _ <- remainder
  return "break"

broken :: Parser String
broken = do
  n <- some digitChar
  char '\''
  char '.'
  _ <- remainder
  return "broke"

unbroken :: Parser String
unbroken = do
  n <- some digitChar
  
  --char '.'
  _ <- remainder
  return "unbroke"

arrow :: Parser String
arrow = do
  string ">>"
  _ <- remainder
  return "arrow"

mistake :: Parser String
mistake = do
  _ <- choice $ map char ['=', chr 65279]
  _ <- remainder
  return "mistake"

-- separator lines ($)
-- initial hash sign is comment if not language
-- determinitives {}
-- phonetic compl. and glosses {+}
-- sign damaged (#)
-- broken away ([])
-- qual: ? (unsure), * (collated)
-- correction: ddd!, ddd!(ORIG)
-- >> means what?
-- tilde means what?
-- num . or '. (latter is "broken")
-- commas


-- accession_no,accounting_period,acquisition_history,alternative_years,ark_number,atf_source,atf_up,author,author_remarks,cdli_collation,cdli_comments,citation,collection,composite_id,condition_description,date_entered,date_of_origin,date_remarks,date_updated,dates_referenced,db_source,designation,dumb,dumb2,electronic_publication,elevation,excavation_no,external_id,findspot_remarks,findspot_square,genre,google_earth_collection,google_earth_provenience,height,id,id_text2,id_text,join_information,language,lineart_up,material,museum_no,object_preservation,object_type,period,period_remarks,photo_up,primary_publication,provenience,provenience_remarks,publication_date,publication_history,published_collation,seal_id,seal_information,stratigraphic_level,subgenre,subgenre_remarks,surface_preservation,text_remarks,thickness,translation_source,width,object_remarks

instance FromNamedRecord Artifact where
  parseNamedRecord m = Artifact
    <$> return Nothing -- id_text, id?
    <*> m C..: "designation" -- primary_publication?
    <*> return Nothing -- language (!= "undetermined")
    <*> return Nothing -- language (!= "undetermined")
    <*> return []
    <*> return []
    <*> return []
    <*> return []

