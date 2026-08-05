{-# LANGUAGE ImplicitParams #-}
module Action (module Action, liftIO) where
import Control.Monad.Trans.State.Strict
    ( get, modify, put, StateT, runStateT )
import Control.Monad.Trans.Reader ( ask, ReaderT, runReaderT )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Node
import Environment
import Value
import Data.List.NonEmpty as L
import Data.ByteString (ByteString)
import Data.Hashable
import Data.Binary (Binary)

type Action vars t = StateT (Environment vars) (ReaderT (Task vars) IO) t
type ActionTask vars = Action vars Bool
type ActionEval vars = (?target :: Node) => (Action vars) (EvalResult, [(Node, Node)])
type ActionSig vars = (Action vars) [ByteString]

data Task vars where
    Task :: { targets :: L.NonEmpty Node, sources :: [Node], action :: ActionTask vars, sign :: ActionSig vars } -> Task vars
    Evaluator :: { target :: Node, sources :: [Node], evaluator :: ActionEval vars, sign :: ActionSig vars} -> Task vars

instance Show (Task vars) where
    show (Task targets sources _ _) = "Task " <> show (L.toList targets) <> " -> " <> show sources
    show (Evaluator target sources _ _) = "Propagator " <> show target <> " -> " <> show sources

data EvalResult = forall a . (Value a, Binary a) => EvalResult a | ResultFailure
noResult = EvalResult ()
deriving instance Show EvalResult

instance Eq (Task vars) where
    (==) :: Task vars -> Task vars -> Bool
    (==) t1 t2 = L.head t1.targets == L.head t2.targets
instance Hashable (Task vars) where
    hashWithSalt salt (Task targets _ _ _) = hashWithSalt salt targets
    hashWithSalt salt (Evaluator target _ _ _) = hashWithSalt salt [target]

gett :: Action vars (Task vars)
gett = lift ask
getenv :: Action vars (Environment vars)
getenv = get
putenv :: Environment vars -> Action vars ()
putenv = put
modenv :: (Environment vars -> Environment vars) -> Action vars ()
modenv = modify
runAction :: Environment vars -> Task vars -> Action vars a -> IO (a, Environment vars)
runAction env task action = runReaderT (runStateT action env) task
