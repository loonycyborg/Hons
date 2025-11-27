{-# LANGUAGE BlockArguments #-}
module CmdLineCompat (module CmdLineCompat, ProcessStatus) where

import Control.Exception (IOException, catch)
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (hashWithSalt))
import System.Posix.PosixString hiding (Fd)
import qualified System.Posix.Types
import System.OsString (OsString)
import System.Exit (ExitCode(..))

import ValueCompat (toPosix, fromNativeBS)
import Data.Foldable (traverse_)
import System.OsPath.Posix (isRelative)
import Data.Traversable (for)
import Data.ByteString (empty)

data Redirect = ToPipe | ToFile OsString
newtype Fd = Fd { fd :: System.Posix.Types.Fd } deriving (Eq, Show, Enum)
instance Hashable Fd where
    hashWithSalt n a = hashWithSalt n (fromEnum a)

type Redirects = HM.HashMap Fd Redirect
type CmdOutput = HM.HashMap Fd OsString

stdout = Fd stdOutput
stderr = Fd stdError
stdin = Fd stdInput

spawn :: NE.NonEmpty PosixString -> Redirects -> IO (ProcessStatus, CmdOutput)
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
        fromNativeBS . foldr (<>) empty . reverse <$> reader [] <* closeFd fd
    Just result <- getProcessStatus True False pid
    return (result, outputs)

success :: ProcessStatus -> Bool
success (Exited ExitSuccess) = True
success _ = False
