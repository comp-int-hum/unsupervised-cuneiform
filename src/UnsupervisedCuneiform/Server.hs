module UnsupervisedCuneiform.Server ( page
                                    --, runArtifactsSession
                                    , app
                                    ) where

import GHC.Generics
import qualified Data.Text as Text
import Effectful.Concurrent.STM
import Effectful
--import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
--import Web.Hyperbole.Data.JSON
import Web.Hyperbole
--import Web.Atomic.CSS
--import Debug.Trace (traceShowId)
import Control.DeepSeq
import UnsupervisedCuneiform.Artifact (Artifact)


-- data AllArtifacts = AllArtifacts
--   deriving (Generic, ViewId)


-- --data ArtifactView = ArtifactView ArtifactId
-- --  deriving (Generic, ViewId)

-- instance (Artifacts :> es) => HyperView AllArtifacts es where
--   type Require AllArtifacts = '[]
  
--   data Action AllArtifacts = ViewAction
--     deriving (Generic, ViewAction)
--   --   = ClearCompleted
--   --   | Filter FilterTodo
--   --   | SubmitTodo
--   --   | ToggleAll FilterTodo
--   --   | SetCompleted FilterTodo Todo Bool
--   --   | Destroy FilterTodo Todo
--   --   deriving (Generic, ViewAction)

--   -- todosView :: FilterTodo -> [Todo] -> View AllTodos ()
--   -- todosView filt todos = do
--   --   todoForm filt
--   --   col $ do
--   --     forM_ todos $ \todo -> do
--   --       hyper (TodoView todo.id) $ todoView filt todo
--   --   statusBar filt todos

-- instance ToParam Artifact where
  
-- instance FromParam Artifact where

-- data Artifacts :: Effect where
--   LoadAll :: Artifacts m [Artifact]
  
-- type instance DispatchOf Artifacts = 'Dynamic

-- runArtifactsSession
--   :: forall es a
--    . (Hyperbole :> es, IOE :> es)
--   => [Artifact]
--   -> Eff (Artifacts : es) a
--   -> Eff es a
-- runArtifactsSession xs = interpret $ \_ -> \case
--   LoadAll -> do
--     pure xs

data ArtifactList = ArtifactList
  deriving (Generic, ViewId)

viewList :: [Artifact] -> View ArtifactList ()
viewList _ = el "test"
  

instance (Concurrent :> es, Reader [Artifact] :> es) => HyperView ArtifactList es where
  data Action ArtifactList
    = ViewList
    deriving (Generic, ViewAction)

  update ViewList = do
    --xs <- ask
    pure $ viewList [] --xs
    
    --n <- modify (+ 1)
    --pure $ viewCount n
  --update Decrement = do
  --  ask
    ---pure ()
    --n <- modify (subtract 1)
    --pure $ viewCount n

--loadAll :: (Artifacts :> es) => Eff es [Artifact]
--loadAll = send LoadAll

-- page :: (Artifacts :> es) => Page es '[AllArtifacts]
-- page = do
--   pure $ el "Hello World"
  -- todos <- Artifacts.loadAll
  -- pure $ exampleLayout (Route.Examples Route.Todos) $ do
  --   example' "Todos" "Example/Page/Todos/Todo.hs" $ do
  --     col ~ embed $ hyper AllTodos $ todosView FilterAll todos

--instance Session Artifacts where
--  sessionKey = "artifacts"
--  cookiePath = Just "/"

-- runTodosSession
--   :: forall es a
--    . (Hyperbole :> es, IOE :> es)
--   => Eff (Todos : es) a
--   -> Eff es a
-- runTodosSession = interpret $ \_ -> \case
--   LoadAll -> do
--     AllTodos todos <- session
--     pure $ M.elems todos
--   Save todo -> do
--     modifySession_ $ insert todo
--   Remove todoId -> do
--     modifySession_ $ delete todoId
--   Create task -> do
--     todoId <- randomId
--     let todo = Todo todoId task False
--     modifySession_ $ insert todo
--     pure todoId
--  where
--   randomId :: (IOE :> es) => Eff es Text
--   randomId = do
--     n <- randomRIO @Int (0, 9999999)
--     pure $ "todo-" <> pack (show n)


page :: (Hyperbole :> es, Concurrent :> es, Reader [Artifact] :> es) => Page es '[ArtifactList]
page = do
  (xs :: [Artifact]) <- ask
  let --xs = []
      n = length xs
      l = show n
      m = text $ Text.pack $ "Hello" ++ l 
  pure $ el $ m
  --hyper ArtifactList (viewList xs)
  
app :: [Artifact] -> Application
app xs = do
  liveApp quickStartDocument (runReader (xs `deepseq` xs) $ runPage page)
-- instance HyperView Artifacts es where
--   data Action Artifacts = List [Int] deriving (Generic, ViewAction)
--   update (List xs) = undefined
