{-# LANGUAGE ImplicitParams #-}
module Action (module Action, module Control.Monad.Trans.State.Strict, module Control.Monad.Trans.Reader, liftIO) where
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

type ActionM vars t = StateT (Environment vars) (ReaderT (Task vars) IO) t
type Action vars = (ActionM vars) Bool
type ActionEval vars = (?target :: Node) => (ActionM vars) (EvalResult, [(Node, Node)])
type ActionSig vars = (ActionM vars) [ByteString]

data Task vars where
    Task :: { targets :: L.NonEmpty Node, sources :: [Node], action :: Action vars, sign :: ActionSig vars } -> Task vars
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

gett :: ActionM vars (Task vars)
gett = lift ask
getenv :: ActionM vars (Environment vars)
getenv = get
putenv :: Environment vars -> ActionM vars ()
putenv = put
modenv :: (Environment vars -> Environment vars) -> ActionM vars ()
modenv = modify
