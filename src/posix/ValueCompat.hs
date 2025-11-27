module ValueCompat where

import System.OsString (coercionToPlatformTypes)
import Data.Type.Coercion (coerceWith)
import Data.ByteString (ByteString)
import System.OsString.Data.ByteString.Short (toShort, fromShort)
import System.OsString.Internal.Types (PosixString(PosixString, getPosixString), OsString (OsString))

toPosix :: OsString -> PosixString
toPosix path = case coercionToPlatformTypes of
    Right (_, coercion) -> coerceWith coercion path

fromNativeBS :: ByteString -> OsString
fromNativeBS = OsString . PosixString . toShort

toNativeBS :: OsString -> ByteString
toNativeBS = fromShort . getPosixString . toPosix
