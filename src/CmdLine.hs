{-# LANGUAGE BlockArguments, ImplicitParams, DataKinds, AllowAmbiguousTypes, RequiredTypeArguments #-}

module CmdLine where

import System.OsString
    ( OsString, coercionToPlatformTypes, intercalate )
import System.OsString.Posix ( PosixString )
import System.OsPath ( encodeFS, decodeFS)
import System.Posix.Process.PosixString
    ( forkProcess, executeFile, getProcessStatus, ProcessStatus(..) )
import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE

import System.IO.Unsafe ( unsafePerformIO )
import Data.Type.Coercion ( coerceWith )
import Data.Foldable ( Foldable(toList), concat )
import GHC.IO.Exception (ExitCode(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Kind (Type)
import System.OsString.Internal.Types (PosixString(getPosixString))
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)

import Node ( Node(ValueNode, FsNode), NodeListNonEmpty, NodeList )
import Environment
import Action (Action, getenv, ActionM, gett, Task (Task))
import Decider (toPosix)
import DepGraph (RuleSet)
import Builder (command)

{-# NOINLINE encodeArg #-}
encodeArg = unsafePerformIO . encodeFS

class Argument a where
    toCmdLine :: a -> [OsString]

instance Argument Int where
    toCmdLine = (:[]) . encodeArg . show

instance Argument TS.ShortText where
    toCmdLine = (:[]) . encodeArg . TS.unpack

instance Argument StrVar where
    toCmdLine (StrVar a) = [a]

instance Argument String where
    toCmdLine = (:[]) . encodeArg

instance Argument a => Argument (Maybe a) where
    toCmdLine (Just a) = toCmdLine a
    toCmdLine Nothing = []

instance Argument Node where
    toCmdLine (FsNode f) = [f]
    toCmdLine (ValueNode _ v) = [encodeArg v]

instance {-# OVERLAPPABLE #-} (Argument a, Foldable f) => Argument (f a) where
    toCmdLine = foldMap toCmdLine

data CmdLine where
    Cmd  :: Argument a => a -> CmdLine
    (:$) :: Argument a => CmdLine -> a -> CmdLine
infixl 5 :$

instance Show CmdLine where
    show (Cmd a) = show (toCmdLine a)
    show (as :$ a) = show as ++ " " ++ show (toCmdLine a)

expand :: CmdLine -> NE.NonEmpty OsString
expand (Cmd a) = NE.fromList . toCmdLine $ a
expand (as :$ a) = NE.appendList (expand as) (toCmdLine a)

expandToStr (Cmd a) = intercalate (encodeArg " ") $ toCmdLine a
expandToStr (as :$ a) = intercalate (encodeArg " ") $ expandToStr as : toCmdLine a

spawn :: NE.NonEmpty PosixString -> IO ProcessStatus
spawn (cmd NE.:| args) = do
    pid <- forkProcess do
        executeFile cmd True args Nothing
    Just result <- getProcessStatus True False pid
    return result

spawnCmd :: CmdLine -> IO ProcessStatus
spawnCmd cmdline =
    let args = expand cmdline
    in
        case coercionToPlatformTypes of
            Right (_, coercion) -> spawn $ fmap (coerceWith coercion) args

spawnCmdPrint cmdline = do
    str <- decodeFS . expandToStr $ cmdline
    putStrLn str
    spawnCmd cmdline

success (Exited ExitSuccess) = True
success _ = False

execute :: MonadIO m => CmdLine -> m Bool
execute = fmap success . liftIO . spawnCmdPrint

inTaskContext :: ((?e::Environment vars, ?t::Task vars) => ActionM vars a) -> ActionM vars a
inTaskContext action = do
    env <- getenv
    task <- gett
    let ?e = env
    let ?t = task
    action

expandForSignature :: CmdLine -> [ByteString]
expandForSignature = toList . fmap (fromShort . getPosixString . toPosix) . expand

subst :: forall {vars} {a} . forall (n :: VarName) -> (LookupType n vars ~ a, ConstructionVariable a, ?e::(Environment vars), VarNameVal n) => a
subst n = eLookup n ?e

substT :: (?t::Task vars) => NE.NonEmpty Node
substT = targets where Task targets _ _ _ = ?t

substS :: (?t::Task vars) => [Node]
substS = sources where Task _ sources _ _ = ?t

osExecute :: ((?e::Environment vars, ?t::Task vars) => CmdLine) -> ActionM vars Bool
osExecute cmdline = inTaskContext do execute cmdline

osCommand :: (NodeListNonEmpty a, NodeList b) => ((?e::Environment vars, ?t::Task vars) => CmdLine) -> a -> b -> RuleSet vars
osCommand cmdline target source = command target source
    (osExecute cmdline)
    (inTaskContext $ return $ expandForSignature cmdline)
