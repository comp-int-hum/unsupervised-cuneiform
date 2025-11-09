module UnsupervisedCuneiform.Domain ( Tablet(..)
                                    , Location(..)
                                    , Person(..)
                                    , Period(..)
                                    , CuneiformDomain
                                    ) where

--import Control.Lens (makeLenses)

-- Bool, String, Int, Double, 3List, List, Tuple, Set, Float

--type Id = String
type Categorical = Int
type Scalar = Float
type Image = [[[Float]]]
type Text = String


data Tablet = Tablet { genre :: Categorical
                     , subgenre :: Categorical
                     , drawing :: Image --Maybe [[[Float]]]
                     , photo :: Image --Maybe [[[Float]]]
                     , width :: Scalar --Maybe Float
                     , height :: Scalar --Maybe Float
                     , thickness :: Scalar --Maybe Float                     
                     , language :: Categorical --Maybe Int
                     , objectType :: Categorical --Maybe Int
                     , material :: Categorical --Maybe Int
                     , content :: Text --Maybe String
                     , discoveredAt :: Location
                     , discoveredBy :: Person
                     , createdIn :: Period
                     }
              
data Location = Location { latitude :: Scalar
                         , longitude :: Scalar
                         }
                
data Person = Person { name :: Text
                     }
              
data Period = Period { start :: Float
                     , end :: Float
                     }

type CuneiformDomain = (Tablet, Location, Person, Period)


