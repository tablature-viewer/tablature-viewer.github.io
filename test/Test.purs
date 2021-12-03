module Test where

import Prelude

import Control.Monad.State (class MonadState, StateT(..), execStateT, state)
import Control.Monad.State as MonadState
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Halogen as H


data Incrementor m = Incrementor (m Unit)
runIncrementor :: forall m. Incrementor m -> m Unit
runIncrementor (Incrementor inc) = inc

newtype MyStateT m a = MyStateT (StateT (State (MyStateT m)) m a)
unMyStateT (MyStateT sm) = sm
derive instance Newtype (MyStateT m a) _
derive instance Functor m => Functor (MyStateT m)
derive newtype instance Monad m => Apply (MyStateT m)
derive newtype instance Monad m => Applicative (MyStateT m)
derive newtype instance Monad m => Bind (MyStateT m)
derive newtype instance Monad m => Monad (MyStateT m)
derive newtype instance MonadEffect m => MonadEffect (MyStateT m)
instance Monad m => MonadState (State (MyStateT m)) (MyStateT m) where
  state f = MyStateT (StateT $ pure <<< f)

newtype MyHaloT m a = MyHaloT (H.HalogenM (State (MyHaloT m)) Unit () Unit m a)
unMyHaloT (MyHaloT m) = m
derive instance Newtype (MyHaloT m a) _
derive instance Functor m => Functor (MyHaloT m)
derive newtype instance Monad m => Apply (MyHaloT m)
derive newtype instance Monad m => Applicative (MyHaloT m)
derive newtype instance Monad m => Bind (MyHaloT m)
derive newtype instance Monad m => Monad (MyHaloT m)
derive newtype instance MonadEffect m => MonadEffect (MyHaloT m)
instance Monad m => MonadState (State (MyHaloT m)) (MyHaloT m) where
  state f = MyHaloT $ state f
instance MonadTrans (MyHaloT) where
  lift ma = MyHaloT $ lift ma

newtype State m = State
  { value :: Int
  , inc :: Incrementor m }

initialState :: forall m . Monad m => State (MyStateT m)
initialState = State
  { value: 0
  , inc: Incrementor $ MonadState.modify_ \(State s) -> State s { value = s.value + 1 }
  }

-- We don't do the work directly on the halogen monad, to avoid unnecessary rendering triggers.
-- We wrap the halogen monad inside another State monad and do the work on the outer state monad.
doThingsInSeparateStateMonad :: forall m . MonadEffect m => MyStateT (MyHaloT m) Unit
doThingsInSeparateStateMonad = do
  State state <- MonadState.get
  Console.log (show state.value)
  runIncrementor state.inc
  State state' <- MonadState.get
  Console.log $ show state'.value

doThings :: forall m . MonadEffect m => MyHaloT m Unit
doThings = do
  currentState :: State (MyHaloT m) <- MyHaloT MonadState.get
  newState <- execStateT (doThingsInSeparateStateMonad # unMyStateT) currentState
  -- Put the state of the outer state monad in the halogen monad
  H.put $ newState

-- test :: forall m . MonadEffect m => MyStateT m Unit
-- test = do
--   State state <- MonadState.get
--   Console.log (show state.value)
--   runIncrementor state.inc
--   State state' <- MonadState.get
--   Console.log $ show state'.value

-- testmain :: Effect Unit
-- testmain = execStateT (unwrap test) initialState *> pure unit

-- import Prelude

-- import Control.Monad.State (class MonadState, execStateT)
-- import Control.Monad.State as MonadState
-- import Effect (Effect)
-- import Effect.Class.Console as Console


-- data Incrementor m = Incrementor (m Unit)
-- runIncrementor :: forall m. Incrementor m -> m Unit
-- runIncrementor (Incrementor inc) = inc

-- data State m = State
--   { value :: Int
--   , inc :: Incrementor m }

-- initialState :: forall m . MonadState (State m) m => State m
-- initialState = State
--   { value: 0
--   , inc: Incrementor $ MonadState.modify_ \(State s) -> State s { value = s.value + 1 }
--   }

-- test :: StateT (State (StateT (State ...) Effect Unit)) Effect Unit
-- test = do
--   State state <- MonadState.get
--   Console.log (show state.value)
--   runIncrementor state.inc
--   State state' <- MonadState.get
--   Console.log $ show state'.value

-- testmain :: Effect Unit
-- testmain = execStateT test initialState *> pure unit



newtype Showable = Showable (forall r. (forall a. Show a => a -> r) -> r)

mkShowable :: forall a. Show a => a -> Showable
mkShowable a = Showable \k -> k a

unShowable :: forall r. (forall a. Show a => a -> r) -> Showable -> r
unShowable k1 (Showable k2) = k2 k1

test1 :: Showable
test1 = mkShowable 42
test2 :: String
-- test2 = unShowable (\a -> show a) test1
test2 = unShowable show test1

data Foo bar = Foo bar | Nothing
data Bar foo = Bar foo | None

newtype X = X (Foo (Bar X))
