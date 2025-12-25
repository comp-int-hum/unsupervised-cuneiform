module UnsupervisedCuneiform.Utils ( Categorical
                                   , Scalar
                                   , Image
                                   , Text
                                   , Related(..)
                                   ) where
import GHC.Types
import Torch.Typed (HList(..))

-- import Torch.Typed

-- train :: forall dtype device n . ( KnownNat n
--                                  , KnownDevice device
--                                  , RandDTypeIsValid device dtype
--                                  , KnownDType dtype
--                                  , StandardFloatingPointDTypeValidation device dtype
--                                  , BasicArithmeticDTypeIsValid device dtype
--                                  , ComparisonDTypeIsValid device dtype
--                                  ) => Proxy n -> IO ()
-- train p = do
--   model <- sample (MLPSpec :: MLPSpec '[ n, 10, n ] dtype device )
--   let initOptim = mkAdam 0 0.9 0.999 (flattenParameters model)
--       learningRate = 0.1
--   (model', opt', _) <- foldM step (model, initOptim, learningRate) [1..100]
--   return ()
--   where
--     step (m, opt, lr) i = do
--       t <- randn :: IO (Tensor device dtype '[20, n])  
--       o <- forwardStoch m t
--       let l = smoothL1Loss @ReduceMean o t
--       print l
--       (m', opt') <- runStep m opt l lr
--       return (m', opt', lr)

type Categorical = Int
type Scalar = Float
type Image = [[[Float]]]
type Text = String
data Related a -- :: (Symbol -> Type)
  
