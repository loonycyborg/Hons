{-# LANGUAGE BlockArguments, ImplicitParams, DataKinds, AllowAmbiguousTypes, RequiredTypeArguments #-}

module CmdLine (module CmdLine, module Value) where

import System.OsString
    ( OsString, coercionToPlatformTypes, intercalate )
import System.OsString.Posix ( PosixString )
import System.OsPath ( encodeFS, decodeFS)
import System.Posix.Process.PosixString
    ( forkProcess, executeFile, getProcessStatus, ProcessStatus(..) )
import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE

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
import Value

data CmdLine where
    Cmd  :: Value a => a -> CmdLine
    (:$) :: Value a => CmdLine -> a -> CmdLine
infixl 5 :$

instance Show CmdLine where
    show (Cmd a) = show (toCmdLine a)
    show (as :$ a) = show as ++ " " ++ show (toCmdLine a)

expand :: CmdLine -> NE.NonEmpty OsString
expand (Cmd a) = NE.fromList . toCmdLine $ a
expand (as :$ a) = NE.appendList (expand as) (toCmdLine a)

expandToStr (Cmd a) = intercalate (encodeVal " ") $ toCmdLine a
expandToStr (as :$ a) = intercalate (encodeVal " ") $ expandToStr as : toCmdLine a

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
