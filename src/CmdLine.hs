{-# LANGUAGE BlockArguments, ImplicitParams, DataKinds, AllowAmbiguousTypes, RequiredTypeArguments #-}

module CmdLine (module CmdLine, module Value) where

import System.OsString
    ( OsString, coercionToPlatformTypes, intercalate )
import System.OsString.Posix ( PosixString )
import System.OsPath ( decodeFS)
import System.Posix.Process.PosixString
    ( forkProcess, executeFile, getProcessStatus, ProcessStatus(..) )
import System.Posix.PosixString (createPipe, dupTo, fdRead, closeFd, createFile, stdFileMode)
import qualified System.Posix.PosixString
import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import Data.Bifunctor
import Data.Hashable
import Data.Traversable (for)
import Control.Exception (catch, IOException)
import Data.Type.Coercion ( coerceWith )
import Data.Foldable ( traverse_ )
import GHC.IO.Exception (ExitCode(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Kind (Type)
import Data.ByteString (ByteString)
import System.OsPath.Posix (isRelative)
import qualified System.Posix.Types

import Node ( Node(ValueNode, FsNode), NodeListNonEmpty, NodeList )
import Environment
import Action (Action, getenv, ActionM, gett, Task (Task))
import DepGraph (RuleSet)
import Builder (command)
import Value

data CmdLine where
    Cmd  :: Value a => a -> CmdLine
    (:$) :: Value a => CmdLine -> a -> CmdLine
    (:>) :: CmdLine -> (Fd, OsString) -> CmdLine
    (:|) :: CmdLine -> Fd -> CmdLine
infixl 5 :$
infixl 5 :>
infixl 5 :|

instance Show CmdLine where
    show (Cmd a) = show (toCmdLine a)
    show (as :$ a) = show as ++ " " ++ show (toCmdLine a)
    show (as :> (fd, file)) = show as ++ " " ++ show (fromEnum fd) ++ "> " ++ show file
    show (as :| fd) = show as ++ " |" ++ show (fromEnum fd)

instance Value CmdLine where
    toCmdLine = NE.toList . fst . expand

data Redirect = ToPipe | ToFile OsString
newtype Fd = Fd { fd :: System.Posix.Types.Fd } deriving (Eq, Show, Enum)
instance Hashable Fd where
    hashWithSalt n a = hashWithSalt n (fromEnum a)

stdout = Fd System.Posix.PosixString.stdOutput
stderr = Fd System.Posix.PosixString.stdError
stdin = Fd System.Posix.PosixString.stdInput

expand :: CmdLine -> (NE.NonEmpty OsString, HM.HashMap Fd Redirect)
expand (Cmd a) = (NE.fromList . toCmdLine $ a, HM.empty)
expand (as :$ a) = first (`NE.appendList` toCmdLine a) (expand as)
expand (as :> (fd, file)) = HM.insert fd (ToFile file) <$> expand as
expand (as :| fd) = HM.insert fd ToPipe <$> expand as

expandToStr (Cmd a) = intercalate (encodeVal " ") $ toCmdLine a
expandToStr (as :$ a) = intercalate (encodeVal " ") $ expandToStr as : toCmdLine a
expandToStr (as :> (fd, file)) = expandToStr as <> encodeVal " " <> encodeVal (show (fromEnum fd)) <> encodeVal "> " <> file
expandToStr (as :| fd) = expandToStr as <> encodeVal " " <> encodeVal (show (fromEnum fd)) <> encodeVal "|"

spawn :: NE.NonEmpty PosixString -> HM.HashMap Fd Redirect -> IO ProcessStatus
spawn (cmd NE.:| args) redirects = do
    redirect_actions <- flip HM.traverseWithKey redirects \fd redirect ->
        case redirect of
            ToPipe -> do
                (readFd, writeFd) <- createPipe
                return (Just (readFd, writeFd), dupTo writeFd fd.fd >> closeFd writeFd >> closeFd readFd)
            ToFile file -> do return (Nothing, do
                                fileFd <- createFile (toPosix file) stdFileMode
                                dupTo fileFd fd.fd
                                closeFd fileFd
                                )
    pid <- forkProcess do
        traverse_ snd redirect_actions
        executeFile cmd (isRelative cmd) args Nothing
    outputs <- for (HM.mapMaybe fst redirect_actions) \(fd, writeFd) -> do
        closeFd writeFd
        let reader l = catch do
                    chunk <- fdRead fd 4096
                    reader $ chunk : l
                (\(e :: IOException) -> return l)
        foldr1 (<>) . reverse <$> reader [] <* closeFd fd
    Just result <- getProcessStatus True False pid
    return result

spawnCmd :: CmdLine -> IO ProcessStatus
spawnCmd cmdline =
    let (args, redirects) = expand cmdline
    in
        case coercionToPlatformTypes of
            Right (_, coercion) -> do
                spawn (coerceWith coercion <$> args) redirects

spawnCmdPrint :: CmdLine -> IO ProcessStatus
spawnCmdPrint cmdline = do
    str <- decodeFS . expandToStr $ cmdline
    putStrLn str
    spawnCmd cmdline

success :: ProcessStatus -> Bool
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
    (inTaskContext $ return $ toSignature cmdline)
