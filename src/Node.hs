{-# LANGUAGE TemplateHaskellQuotes, TypeFamilies #-}

module Node where
import System.OsPath
import System.Directory.OsPath
import Algebra.Graph.ToGraph ( ToGraph(ToVertex, toGraph) )
import Algebra.Graph
import GHC.IO (unsafePerformIO)
import Data.Hashable ( Hashable(hashWithSalt) )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax ( Lift(lift, liftTyped) )
import qualified Control.Monad.Trans.State.Strict as State
import Data.Typeable
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Foldable1 (Foldable1 (foldMap1))
import qualified Data.List.NonEmpty as NE
import Environment
import Debug.Trace (trace)
import System.OsString (encodeLE)

data Node where
    FsNode :: { path :: OsPath } -> Node
    ValueNode :: forall a . EnvTransform a => { name :: String, value :: String, transform :: a } -> Node

instance Eq Node where
    FsNode p1 == FsNode p2 = p1 == p2
    ValueNode n1 v1 _ == ValueNode n2 v2 _ = n1 == n2 && v1 == v2
    n1 == n2 = False
instance Ord Node where
    FsNode p1 `compare` FsNode p2 = p1 `compare` p2
    ValueNode n1 _ _ `compare` ValueNode n2 _ _ = n1 `compare` n2
    x `compare` y = prio x `compare` prio y where
        prio (ValueNode {}) = 1
        prio (FsNode {}) = 0

instance Lift Node where
    lift (FsNode p) = [| FsNode p |]
    lift (ValueNode n v _) = [| ValueNode n v EIdentity |]
    liftTyped (FsNode p) = [|| FsNode p ||]
    liftTyped (ValueNode n v _) = [|| ValueNode n v EIdentity ||]

instance Hashable Node where
    hashWithSalt salt (FsNode path) = hashWithSalt salt path
    hashWithSalt salt (ValueNode name _ _) = hashWithSalt salt name

instance Show Node where
    show (FsNode path) = "[fs|" ++ (unsafePerformIO . decodeFS $ path) ++ "|]"
    show (ValueNode name value _) = "[value|" ++ value ++ "|]"

class NodeList l where
    toList :: l -> [Node]

instance NodeList Node where
    toList = (:[])

instance Foldable f => NodeList (f Node) where
    toList = concatMap (:[])

class NodeList l => NodeListNonEmpty l where
    toNonEmpty :: l -> NonEmpty Node

instance NodeListNonEmpty Node where
    toNonEmpty =  (:|[])

instance Foldable1 f => NodeListNonEmpty (f Node) where
    toNonEmpty = foldMap1 (:|[])

encodeFilename = unsafePerformIO . encodeLE

{-# NOINLINE baseDir #-}
baseDir :: OsPath
baseDir = unsafePerformIO . canonicalizePath . unsafeEncodeUtf $ "."
mkFsNode :: OsPath -> Node
mkFsNode = FsNode . makeRelative baseDir . unsafePerformIO . canonicalizePath
mkAlias :: String -> Node
mkAlias a = ValueNode a "" EDropOverrides
mkValue :: String -> Node 
mkValue name = ValueNode name name EIdentity
mkPropagator :: Typeable vars => String -> Environment vars -> State.State (Environment vars) () -> Node
mkPropagator name env st = ValueNode name name (Environment.EStateTransform st) 

fs :: QuasiQuoter
fs = QuasiQuoter {
    quoteExp = \x -> do
        let file' = mkFsNode . encodeFilename $ x
        lift file',
    quotePat = \_ -> fail "filelist quasiquoter doesn't support use as pattern",
    quoteType = \_ -> fail "filelist quasiquoter doesn't support use as type",
    quoteDec = \_ -> fail "filelist quasiquoter doesn't support use as declaration"
}

filelist :: QuasiQuoter
filelist = QuasiQuoter {
    quoteExp = \x -> do
        let filelist' = NE.map (mkFsNode . encodeFilename) (fromList . words $ x)
        lift filelist',
    quotePat = \_ -> fail "filelist quasiquoter doesn't support use as pattern",
    quoteType = \_ -> fail "filelist quasiquoter doesn't support use as type",
    quoteDec = \_ -> fail "filelist quasiquoter doesn't support use as declaration"
}
