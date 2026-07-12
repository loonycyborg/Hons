{-# LANGUAGE BlockArguments, ImplicitParams, DataKinds, AllowAmbiguousTypes, RequiredTypeArguments #-}

module CmdLine (module CmdLine, module Value) where

import System.OsString
    ( OsString, coercionToPlatformTypes, intercalate )
import System.OsPath ( decodeFS)
import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import Data.Bifunctor
import Data.Type.Coercion ( coerceWith )
import Data.Maybe (isJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Kind (Type)

import Node ( Node(ValueNode, FsNode), NodeListNonEmpty, NodeList, toNonEmpty )
import Environment
import Action (Action, getenv, ActionM, gett, Task (Task, Evaluator))
import DepGraph (RuleSet)
import Builder (command, Builder (..), ChainType(..), Tag (..))
import Value
import CmdLineCompat

data CmdLine where
    Cmd  :: Value a => a -> CmdLine
    (:$) :: Value a => CmdLine -> a -> CmdLine
    (:>) :: CmdLine -> (Fd, OsString) -> CmdLine
    (:|) :: CmdLine -> Fd -> CmdLine
    (:@) :: CmdLine -> CmdLog -> CmdLine
infixl 5 :$
infixl 5 :>
infixl 5 :|
infixl 5 :@

instance Show CmdLine where
    show (Cmd a) = show (toCmdLine a)
    show (as :$ a) = show as ++ " " ++ show (toCmdLine a)
    show (as :> (fd, file)) = show as ++ " " ++ show (fromEnum fd) ++ "> " ++ show file
    show (as :| fd) = show as ++ " |" ++ show (fromEnum fd)

outFile :: Node -> (Fd, OsString)
outFile (FsNode name) = (stdout, name)

instance Value CmdLine where
    toCmdLine = NE.toList . fst . expand

data CmdLog = LogStdout | LogSilent deriving (Show, Eq)

expand :: CmdLine -> (NE.NonEmpty OsString, Redirects)
expand (Cmd a) = (NE.fromList . toCmdLine $ a, HM.empty)
expand (as :$ a) = (`NE.appendList` toCmdLine a) `first` expand as
expand (as :> (fd, file)) = HM.insert fd (ToFile file) <$> expand as
expand (as :| fd) = HM.insert fd ToPipe <$> expand as
expand (as :@ _) = expand as

expandToStr :: CmdLine -> (OsString, CmdLog)
expandToStr (Cmd a)            = (intercalate (encodeVal " ") $ toCmdLine a, LogStdout)
expandToStr (as :$ a)          = (intercalate (encodeVal " ") . (:toCmdLine a))                                 `first` expandToStr as
expandToStr (as :> (fd, file)) = (<> encodeVal " " <> encodeVal (show (fromEnum fd)) <> encodeVal "> " <> file) `first` expandToStr as
expandToStr (as :| fd)         = (<> encodeVal " " <> encodeVal (show (fromEnum fd)) <> encodeVal "|")          `first` expandToStr as
expandToStr (as :@ log)        = second (const log) (expandToStr as)

spawnCmd :: CmdLine -> IO (ProcessStatus, CmdOutput)
spawnCmd cmdline =
    let (args, redirects) = expand cmdline
    in
        case coercionToPlatformTypes of
            Right (_, coercion) -> do
                spawn (coerceWith coercion <$> args) redirects

spawnCmdPrint :: CmdLine -> IO (ProcessStatus, CmdOutput)
spawnCmdPrint cmdline = do
    let (str, log) = expandToStr cmdline
    when (log == LogStdout) do
        decodeFS str >>= putStrLn
    spawnCmd cmdline

execute :: MonadIO m => CmdLine -> m (Maybe CmdOutput)
execute cmdline = do
    (result, output) <- liftIO $ spawnCmdPrint cmdline
    return if success result then Just output else Nothing

inTaskContext :: ((?e::Environment vars, ?t::Task vars) => ActionM vars a) -> ActionM vars a
inTaskContext action = do
    env <- getenv
    task <- gett
    let ?e = env
    let ?t = task
    action

subst :: forall {vars} {a} . forall (n :: VarName) -> (LookupType n vars ~ a, ConstructionVariable a, ?e::Environment vars, VarNameVal n) => a
subst n = eLookup n ?e

substL :: forall (n :: Symbol) -> (LookupType (LocalVar n) vars ~ a, ConstructionVariable a, ?e::Environment vars, KnownSymbol n) => a
substL l = subst (LocalVar l)

substT :: (?t::Task vars) => NE.NonEmpty Node
substT = targets where
    targets = case ?t of
        Task targets _ _ _      -> targets
        Evaluator target _  _ _ -> NE.singleton target

substTarget :: (?t::Task vars) => Node
substTarget = NE.head substT

substS :: (?t::Task vars) => [Node]
substS = sources where
    sources = case ?t of
        Task _ sources _ _      -> sources
        Evaluator _ sources _ _ -> sources

osExecute :: ((?e::Environment vars, ?t::Task vars) => CmdLine) -> ActionM vars (Maybe CmdOutput)
osExecute cmdline = inTaskContext do execute cmdline

osExecutePipeStdout :: ((?e::Environment vars, ?t::Task vars) => CmdLine) -> ActionM vars (Maybe OsString)
osExecutePipeStdout cmdline = do
    output <- osExecute $ cmdline :| stdout
    return $ (HM.! stdout) <$> output

osCommand :: (NodeListNonEmpty a, NodeList b) => ((?e::Environment vars, ?t::Task vars) => CmdLine) -> a -> b -> RuleSet vars
osCommand cmdline = command
    (isJust <$> osExecute cmdline)
    (inTaskContext $ return $ toSignature cmdline)

osBuilder :: NodeListNonEmpty t => ((?e::Environment vars, ?t::Task vars) => CmdLine) -> t -> NE.NonEmpty (Builder vars BuilderC) -> Builder vars BuilderC
osBuilder cmdline = Builder TagNihil (osCommand cmdline) toNonEmpty id
