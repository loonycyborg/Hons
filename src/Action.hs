module Action (module Action, module Control.Monad.Trans.State.Strict, module Control.Monad.Trans.Reader, liftIO) where
import Control.Monad.Trans.State.Strict
    ( get, modify, put, StateT, runStateT )
import Control.Monad.Trans.Reader ( ask, ReaderT, runReaderT )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Node
import Environment
import Data.List.NonEmpty as L
import Data.ByteString (ByteString)
import Data.Hashable

type ActionM vars t = StateT (Environment vars) (ReaderT (Task vars) IO) t
type Action vars = (ActionM vars) Bool

data Task vars where
    Task :: { targets :: L.NonEmpty Node, sources :: [Node], action :: Action vars, sign :: (ActionM vars) [ByteString] } -> Task vars

instance Eq (Task vars) where
    (==) :: Task vars -> Task vars -> Bool
    (==) t1 t2 = L.head t1.targets == L.head t2.targets

instance Show (Task vars) where
    show (Task targets _ _ _) = "[[[" ++ (show . L.head $ targets) ++ "]]]"

instance Hashable (Task vars) where
    hashWithSalt salt t = hashWithSalt salt t.targets

gett :: ActionM vars (Task vars)
gett = lift ask
getenv :: ActionM vars (Environment vars)
getenv = get
putenv :: Environment vars -> ActionM vars ()
putenv = put
modenv :: (Environment vars -> Environment vars) -> ActionM vars ()
modenv = modify
