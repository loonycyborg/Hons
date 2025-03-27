{-# LANGUAGE GADTs, FlexibleInstances, BlockArguments #-}

module CmdLine where

import System.OsString
    ( OsString, coercionToPlatformTypes, encodeLE, decodeLE, intercalate )
import System.OsString.Posix ( PosixString )
import System.Posix.Process.PosixString
    ( forkProcess, executeFile, getProcessStatus, ProcessStatus(..) )
import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE

import System.IO.Unsafe ( unsafePerformIO )
import Data.Type.Coercion ( coerceWith )
import Data.Foldable ( Foldable(toList), concat )

import Node ( Node(ValueNode, FsNode) )
import GHC.IO.Exception (ExitCode(..))

{-# NOINLINE encodeArg #-}
encodeArg = unsafePerformIO . encodeLE

class Argument a where
    toCmdLine :: a -> [OsString]

instance Argument Int where
    toCmdLine = (:[]) . encodeArg . show

instance Argument TS.ShortText where
    toCmdLine = (:[]) . encodeArg . TS.unpack

instance Argument String where
    toCmdLine = (:[]) . encodeArg

instance Argument a => Argument (Maybe a) where
    toCmdLine (Just a) = toCmdLine a
    toCmdLine Nothing = []

instance Argument Node where
    toCmdLine (FsNode f) = [f]
    toCmdLine (ValueNode _ v _) = [encodeArg v]

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
expand (as :$ a) = expand as <> NE.fromList (toCmdLine a)

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
    str <- decodeLE . expandToStr $ cmdline
    putStrLn str
    spawnCmd cmdline

success (Exited ExitSuccess) = True
success _ = False
