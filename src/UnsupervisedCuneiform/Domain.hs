module UnsupervisedCuneiform.Domain ( Cuneiform(..)
                                    , Domain
                                    , PropertyList
                                    ) where

import UnsupervisedCuneiform.Utils ( Categorical
                                   , Image
                                   , Scalar
                                   , Text
                                   , Related(..)
                                   )
import GHC.TypeLits (Symbol)
import GHC.Types (Type)

type PropertyList = [ ( Symbol, Type ) ]
type Domain = [ ( Symbol, PropertyList ) ]


type Cuneiform = '[ '( "tablet"
                     , '[ '( "genre", Categorical )                       
                        , '( "subgenre", Categorical )
                        , '( "drawing", Image )
                        , '( "width", Scalar )
                        , '( "height", Scalar )
                        , '( "thickness", Scalar )
                        , '( "language", Categorical )
                        , '( "objectType", Categorical )
                        , '( "material", Categorical )
                        , '( "content", Text )
                        , '( "discoveredBy", Related "person" )
                        , '( "discoveredAt", Related "location" )
                        , '( "createdIn", Related "period" )
                        ]
                     )
                  , '( "location"
                     , '[ '( "latitude", Scalar )
                        , '( "longitude", Scalar )
                        ]
                     )
                  , '( "person"
                     , '[ '( "name", Text )
                        , '( "bornAt", Related "location" )
                        ]
                     )
                  , '( "period"
                     , '[ '( "start", Scalar )
                        , '( "end", Scalar )
                        ]
                     )
                  ]
