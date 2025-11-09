module UnsupervisedCuneiform.Corpus ( Corpus(..)
                                    ) where

import Control.Lens (makeLenses, (^.), (.~), (&)) --, (.~), (<&>), set, view, makeLenses, makeFields)
import UnsupervisedCuneiform.Artifact (Artifact(..))

newtype Category = Category String


data Entity = Tablet { _genre :: Category
                     }
            | Location {
                       }
            | Person {
                     }
            | Period {
                     }
            
toCorpus :: [Artifact] -> Corpus
toCorpus xs = undefined


makeLenses ''Corpus


