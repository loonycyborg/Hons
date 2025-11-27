module FileCompat where

import Data.Int (Int64)
import System.OsPath (OsPath)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import System.Posix.PosixString (getFileStatus, modificationTimeHiRes)

import ValueCompat (toPosix)

gainTimestamp :: IO Int64
gainTimestamp = mkTimestamp <$> getPOSIXTime

mkTimestamp :: NominalDiffTime -> Int64
mkTimestamp t = floor $ nominalDiffTimeToSeconds t * 1e9

timestampFile :: OsPath -> IO Int64
timestampFile path = mkTimestamp . modificationTimeHiRes <$> (getFileStatus . toPosix) path
