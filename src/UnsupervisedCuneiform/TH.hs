module UnsupervisedCuneiform.TH ( makeModel
                                ) where

import Prelude hiding (tanh)
import Data.Char (toLower, toUpper)
--import Data.Proxy
--import Foreign.ForeignPtr
import GHC.Generics hiding (NoSourceUnpackedness, NoSourceStrictness)
import GHC.TypeLits
--import GHC.TypeLits.Extra
--import System.Environment
--import System.IO.Unsafe
import Torch.Internal.Managed.Type.Context (manual_seed_L)
import Torch.Typed
import Torch.Typed (DType, DeviceType, Dropout(..))
import Torch.DType
import Torch.Initializers (NonLinearity(..))
import Control.Monad
import Language.Haskell.TH
import Effectful
import UnsupervisedCuneiform.MLP (MLP(..))
--import UnsupervisedCuneiform.MLP (MLPSpec(..), MLP(..))
--import UnsupervisedCuneiform.CNN (CNNSpec(..), CNN(..))
--import UnsupervisedCuneiform.RNN (RNNSpec(..), RNN(..))
--import UnsupervisedCuneiform.AutoEncoder (AutoEncoderSpec(..), AutoEncoder(..))
--import UnsupervisedCuneiform.GCN (GCNSpec(..), GCN(..))
--import UnsupervisedCuneiform.StarCoder (StarCoderSpec(..), StarCoder(..))


--import UnsupervisedCuneiform.TemplateConfig (TemplateConfig(..))

{-
each property p on entity-type e needs:

  an encoder transforming a value of p to a penc-size tensor (plus suitable dims)
  a decoder transforming a tensor of e_out-size tensor to a value of p (plus suitable dims)

each relationship r(a, b), e.g. 'drove(Person, Car)', needs:

  an RNN to summarize a sequence of a_bottleneck-size tensors to an rsubj_summary-size tensor (plus suitable dims)
  an RNN to summarize a sequence of b_bottleneck-size tensors to a robj-summary-size tensor (plus suitable dims)
  an adjacency tensor of shape batch_size x batch_size

each entity-type e needs an initial autoencoder eauto(0) with:

  sizes eauto_input(0) and eauto_output(0) of sum(penc-size for all p in e)
  bottleneck size eauto_bottle(0)
  suitable dims

each level of depth d>0 for entity-type e needs an autoencoder eauto(d) with:

  input size eauto(d-1) + sum(sizes of relevant rsubj/robj-summaries)
  output size eauto(d)
  bottleneck size eauto_bottle(d)
  suitable dims

-}


        


lc :: String -> String
lc (c:s) = (toLower c):s

unwrapTypeList ts = go ts []
  where
    go (AppT rest (ConT tp)) acc = go rest (tp:acc)
    go (TupleT _) acc = acc
    go _ acc = acc

dtypeTV n = do
  TyConI (DataD [] vv _ _ _ _) <- reify ''DType
  
  return $ KindedTV n' BndrReq (ConT ''DType)
  where
    n' = (mkName . lc) $ (nameBase n) ++ "Dtype"

deviceTV n = do
  return $ KindedTV n' BndrReq (AppT (AppT (TupleT 2) (ConT ''DeviceType)) (ConT ''Nat))
  where
    n' = (mkName . lc) $ (nameBase n) ++ "Device"

natTV n = do
  return $ KindedTV n BndrReq (ConT ''Nat)
  --where
  --  n' = (mkName . lc) $ (nameBase n) ++ s


nest n tvs = go tvs n'
  where
    n' = (ConT n)
    go [] acc = acc
    go (x:xs) acc = go xs (AppT acc (VarT x))


makeModel :: Name -> Int -> Q [Dec]
makeModel n d = do
  let base@(c:cs) = nameBase n
      specName = mkName $ base ++ "ModelSpec"
      modelName = mkName $ base ++ "Model"
      funcName = mkName $ (toLower c : cs) ++ "Model"
  
  TyConI (TySynD _ _ ts) <- reify n
  dataT@(KindedTV dn _ _) <- dtypeTV n
  deviceT@(KindedTV dvn _ _) <- deviceTV n  
  ents <- (liftM concat . sequence) $ map (processEntityType dn dvn) (unwrapTypeList ts)
  


  let otherT = concat $ [a | (a, _, _) <- ents]
      specs = concat $ [a | (_, a, _) <- ents]
      args = concat $ [a | (_, _, a) <- ents]
      specTypeParamKinds = [dataT, deviceT] ++ otherT
      typeParamNames = [x | KindedTV x _ _ <- specTypeParamKinds]
      specContext = []
      specKind = Nothing
      conctx = [] --[(mlpDropoutProbSpec_6989586621679030932,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Double)]
      specConstructors = [ForallC [PlainTV t SpecifiedSpec | t <- typeParamNames] conctx (RecGadtC [specName] [] (nest specName typeParamNames))]
      specDerivations = [DerivClause Nothing [ConT ''Show]]
      spec = DataD specContext specName specTypeParamKinds specKind specConstructors specDerivations
      
      modelTypeParamKinds = [dataT, deviceT] ++ otherT
      modelContext = []
      modelKind = Nothing
      modelConstructors = [ForallC [PlainTV t SpecifiedSpec | t <- typeParamNames] conctx (RecGadtC [modelName] args (nest modelName typeParamNames))]
      modelDerivations = [DerivClause Nothing [ConT ''Show, ConT ''Generic]] --, ConT ''Parameterized]]
      model = DataD modelContext modelName modelTypeParamKinds modelKind modelConstructors modelDerivations

      --forward = FunD name [Clause]
      --rand = InstanceD (Maybe Overlap) Cxt Type [Dec]
      -- default___

      
  return $ [spec, model]

-- Categorical Scalar Image Text

processEntityType dn dvn n = do
  let entityTypeName@(c:cs) = nameBase n
  TyConI (DataD _ _ _ _ [RecC _ vs] _) <- reify n
  sequence $ map (processField dn dvn (toLower c : cs)) vs

stringify (ConT n) = [nameBase n]
stringify (AppT (ConT n) (ConT m)) = [nameBase n, nameBase m]
stringify (AppT (ConT n) (AppT _ _)) = [nameBase n, "List"]
stringify _ = []


--makeMLP :: String -> Maybe [Int] -> NonLinearity -> Q ([TyVarBndr BndrVis], [VarBangType], [VarBangType])
--makeMLP base layers nl = return ([], [], [])


--makeAutoencoder :: String -> Maybe [Int] -> Maybe Int -> Maybe [Int] -> NonLinearity -> Q ([TyVarBndr BndrVis], [VarBangType], [VarBangType])
--makeAutoencoder base encoder bottleneck decoder nl = return ([], [], [])

--makeLstm :: String -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe RNNDirectionality -> Maybe Double -> NonLinearity -> Q ([TyVarBndr BndrVis], [VarBangType], [VarBangType])
--makeLstm base input hidden numLayers direction dropout nl = return ([], [], [])

--makeCnn :: String -> Q ()

--   let mlp = mkName $ name' ++ "MLP"
-- mlpSpec = mkName $ name' ++ "MLPSpec"
--                                                                 mlpLayerSizes = mkName $ name' ++ "MLPLayerSizes"
--                                                             return ( [ KindedTV mlpLayerSizes BndrReq (AppT (ConT ''[]) (ConT ''Nat)) ]
--                                                                    , []
--                                                                    , []
--                                                                    )

-- depth-wise list
-- adjacency matrices


-- listOfTypeParams, listOfSubSpecs, listOfConstructors,
processField :: Name -> Name -> String -> (Name, Bang, Type) -> Q ([TyVarBndr BndrVis], [VarBangType], [VarBangType])
processField dn dvn prefix (name, bang, tp) = case tp' of ["Categorical_"] -> do
                                                            let emb = mkName $ name' ++ "Embedding"
                                                                embSpec = mkName $ name' ++ "EmbeddingSpec"
                                                                embPad = mkName $ name' ++ "Padding"
                                                                embCount = mkName $ name' ++ "EmbeddingCount"
                                                                embSize = mkName $ name' ++ "EmbeddingSize"
                                                                embType = mkName $ name' ++ "EmbeddingType"
                                                                --argname = mkName $ name' ++ "Embedding"
                                                                --specname = mkName $ name' ++ "EmbeddingSpec"
                                                                embTV = [embPad, embCount, embSize, embType, dn, dvn]
                                                                --tp = nest ''EmbeddingSpec [padname, ecname, ename, etype, dn, dvn]
                                                            return ( [ KindedTV embSize BndrReq (ConT ''Nat)
                                                                     , KindedTV embCount BndrReq (ConT ''Nat)
                                                                     , KindedTV embType BndrReq (ConT ''EmbeddingType)
                                                                     , KindedTV embPad BndrReq (AppT (ConT ''Maybe) (ConT ''Nat))
                                                                     ]
                                                                   , [ ( embSpec
                                                                       , Bang NoSourceUnpackedness NoSourceStrictness
                                                                       , nest ''EmbeddingSpec embTV
                                                                       )
                                                                     ]
                                                                   , [ ( emb
                                                                       , Bang NoSourceUnpackedness NoSourceStrictness
                                                                       , nest ''Embedding embTV
                                                                       )
                                                                     ]
                                                                   )                                                      
                                                          ["Scalar"] -> do
                                                            -- MLP
                                                            let mlp = mkName $ name' ++ "MLP"
                                                                mlpSpec = mkName $ name' ++ "MLPSpec"
                                                                mlpLayerSizes = mkName $ name' ++ "MLPLayerSizes"
                                                            return ( [ KindedTV mlpLayerSizes BndrReq (AppT (ConT ''[]) (ConT ''Nat)) ]
                                                                   , []
                                                                   , []
                                                                   )
                                                          ["Text_"] -> do
                                                            let emb = mkName $ name' ++ "Embedding"
                                                                embSpec = mkName $ name' ++ "EmbeddingSpec"
                                                                embSize = mkName $ name' ++ "EmbeddingSize"
                                                                embCount = mkName $ name' ++ "EmbeddingCount"
                                                                embType = mkName $ name' ++ "EmbeddingType"
                                                                embPad = mkName $ name' ++ "Padding"
                                                                embTV = [embPad, embCount, embSize, embType, dn, dvn]
                                                                
                                                                rnn = mkName $ name' ++ "RNN"
                                                                rnnSpec = mkName $ name' ++ "RNNSpec"
                                                                rnnHiddenSize = mkName $ name' ++ "HiddenSize"
                                                                rnnNumLayers = mkName $ name' ++ "NumLayers"
                                                                rnnDirectionality = mkName $ name' ++ "Directionality"
                                                                rnnTV = [embSize, rnnHiddenSize, rnnNumLayers, rnnDirectionality, dn, dvn]
                                                                
                                                            return ( [ KindedTV embSize BndrReq (ConT ''Nat)
                                                                     , KindedTV embCount BndrReq (ConT ''Nat)
                                                                     , KindedTV embType BndrReq (ConT ''EmbeddingType)
                                                                     , KindedTV embPad BndrReq (AppT (ConT ''Maybe) (ConT ''Nat))
                                                                     , KindedTV rnnHiddenSize BndrReq (ConT ''Nat)
                                                                     , KindedTV rnnNumLayers BndrReq (ConT ''Nat)
                                                                     , KindedTV rnnDirectionality BndrReq (ConT ''RNNDirectionality)
                                                                     ]
                                                                   , [ ( embSpec
                                                                       , Bang NoSourceUnpackedness NoSourceStrictness
                                                                       , nest ''EmbeddingSpec embTV
                                                                       )
                                                                     , ( rnnSpec
                                                                       , Bang NoSourceUnpackedness NoSourceStrictness
                                                                       , nest ''LSTMSpec rnnTV
                                                                       )                                                                       
                                                                     ]
                                                                   , [ ( emb
                                                                       , Bang NoSourceUnpackedness NoSourceStrictness
                                                                       , nest ''Embedding embTV
                                                                       )
                                                                     , ( rnn
                                                                       , Bang NoSourceUnpackedness NoSourceStrictness
                                                                       , nest ''LSTM rnnTV
                                                                       )                                                                       
                                                                     ]
                                                                   )                                                       
                                                          ("Image_":_) -> do
                                                            -- CNN
                                                            let cnnInChannels = mkName $ name' ++ "InputChannelSizes"
                                                                cnnWidths = mkName $ name' ++ "KernelWidths"
                                                                cnnHeights = mkName $ name' ++ "KernelHeights"
                                                                cnnOutChannels = mkName $ name' ++ "OutputChannelSizes"
                                                            return ( [ KindedTV cnnInChannels BndrReq (AppT (ConT ''[]) (ConT ''Nat))
                                                                     , KindedTV cnnWidths BndrReq (AppT (ConT ''[]) (ConT ''Nat))
                                                                     , KindedTV cnnHeights BndrReq (AppT (ConT ''[]) (ConT ''Nat))
                                                                     , KindedTV cnnOutChannels BndrReq (AppT (ConT ''[]) (ConT ''Nat))
                                                                     ]
                                                                   , []
                                                                   , []
                                                                   )
                                                          _ -> do
                                                            let sname = mkName $ name' ++ "SubjectSummarySize"
                                                                oname = mkName $ name' ++ "ObjectSummarySize"
                                                         
                                                            return ( [ KindedTV sname BndrReq (ConT ''Nat)
                                                                     , KindedTV oname BndrReq (ConT ''Nat)
                                                                     ]
                                                                   , []
                                                                   , []
                                                                   )
                                                     
                                                     --runIO $ print (name, tp)
                                                     --return ([], [], [])
                                                     
  where
    tp' = stringify tp
    c:cs = nameBase name
    name' = prefix ++ (toUpper c : cs)
