module UnsupervisedCuneiform.Artifact ( Artifact(..)
                                      , identifier
                                      , designation
                                      , language
                                      , photos
                                      , drawings
                                      , oraccContent
                                      , atfContent
                                      , objectType
                                      , def
                                      , mergeArtifacts
                                      , toJsonL
                                      , fromJsonL
                                      ) where

import Control.DeepSeq (NFData)
import Data.Word
import Data.ByteString (ByteString, intercalate, toStrict, split, fromStrict)
import Data.Maybe (catMaybes)
import Control.Lens (makeLenses, (^.), (.~), (&)) --, (.~), (<&>), set, view, makeLenses, makeFields)
--import Codec.Picture.Types (DynamicImage) --, Image(..), dynamicMap, dynamicPixelMap)
import Data.Default (Default(..))
--import Data.Ord (Ordering(..))
import GHC.Generics
import Data.Aeson (FromJSON(..), ToJSON(..), genericToEncoding, defaultOptions, encode, decode)
--import UnsupervisedCuneiform.Entity (Entity(..))
--import GHC.IO.Handle (Handle)
--import Data.Csv (FromNamedRecord(..))
--import qualified Data.Csv as C
--import Torch.Tensor (Tensor, shape)

-- accession_no,accounting_period,acquisition_history,alternative_years,ark_number,atf_source,atf_up,author,author_remarks,cdli_collation,cdli_comments,citation,collection,composite_id,condition_description,date_entered,date_of_origin,date_remarks,date_updated,dates_referenced,db_source,designation,dumb,dumb2,electronic_publication,elevation,excavation_no,external_id,findspot_remarks,findspot_square,genre,google_earth_collection,google_earth_provenience,height,id,id_text2,id_text,join_information,language,lineart_up,material,museum_no,object_preservation,object_type,period,period_remarks,photo_up,primary_publication,provenience,provenience_remarks,publication_date,publication_history,published_collation,seal_id,seal_information,stratigraphic_level,subgenre,subgenre_remarks,surface_preservation,text_remarks,thickness,translation_source,width,object_remarks
--data Tablet = Tablet { _cdliId :: Text
--                     ,

--data CDL = CDL { _text :: Maybe String
--               } deriving (Show)


data Artifact = Artifact { _identifier :: Maybe String
                         , _designation :: Maybe String
                         , _language :: Maybe String
                         , _objectType :: Maybe String
                         , _photos :: [[[[Float]]]]
                         , _drawings :: [[[[Float]]]]
                         , _atfContent :: [String]
                         , _oraccContent :: [String]
                         -- , _compositeIds :: [String]
                         --, _genre :: Maybe String
                         --, _excavationNo :: Maybe String
                         --, _height/width/thickness :: Maybe Int
                         --, _material :: Maybe String
                         
                         --, _period (numeric range)
                         --, _provenience :: Maybe String
                         --, _subgenre :: Maybe String
                         } deriving (Generic)

instance NFData Artifact

instance ToJSON Artifact where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Artifact where

makeLenses ''Artifact

instance Default Artifact where
  def = Artifact Nothing Nothing Nothing Nothing [] [] [] []

instance Show Artifact where
  show a = show $ (a ^. identifier, a ^. designation, a ^. objectType, a ^. language, map length $ a ^. photos, map length $ a ^. drawings, length $ a ^. oraccContent, length $ a ^. atfContent)

instance Eq Artifact where
  (==) a b = idBased || designationBased
    where
      aId = a ^. identifier
      bId = b ^. identifier
      aDes = a ^. designation
      bDes = b ^. designation
      idBased = (aId /= Nothing) && (aId == bId)
      designationBased = (aDes /= Nothing) && (aDes == bDes)

instance Ord Artifact where
  compare a b = case (aI, bI, aD, bD) of (Just _, Just _, _, _) -> compare aI bI
                                         (_, _, Just _, Just _) -> compare aD bD
                                         _ -> compare (aI, aD) (bI, bD)
                                         
    where
      aI = a ^. identifier
      bI = b ^. identifier
      aD = a ^. designation
      bD = b ^. designation

toJsonL :: [Artifact] -> ByteString
toJsonL = (intercalate "\n" . map (toStrict . encode))

fromJsonL :: ByteString -> [Artifact]
fromJsonL bs = catMaybes $ map (decode . fromStrict) (split ((fromIntegral . fromEnum) '\n' :: Word8) bs)    

mergeArtifacts :: Artifact -> Artifact -> Artifact
mergeArtifacts a b = a & identifier .~ identifier' & designation .~ designation' & language .~ language' & objectType .~ objectType' & photos .~ photos' & drawings .~ drawings' & oraccContent .~ oraccContent' & atfContent .~ atfContent'
  where
    aId = a ^. identifier
    bId = b ^. identifier
    aDes = a ^. designation
    bDes = b ^. designation
    aLang = a ^. language
    bLang = b ^. language
    aTp = a ^. objectType
    bTp = b ^. objectType
    aPs = a ^. photos
    bPs = b ^. photos
    aDs = a ^. drawings
    bDs = b ^. drawings
    aoC = a ^. oraccContent
    boC = b ^. oraccContent
    aaC = a ^. atfContent
    baC = b ^. atfContent    
    identifier' = if aId == Nothing then bId else aId
    language' = if aLang == Nothing then bLang else aLang
    designation' = if aDes == Nothing then bDes else aDes
    objectType' = if aTp == Nothing then bTp else aTp
    photos' = aPs ++ bPs
    drawings' = aDs ++ bDs
    oraccContent' = aoC ++ boC
    atfContent' = aaC ++ baC
