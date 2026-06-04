{-# LANGUAGE UndecidableInstances #-}
module Value (module Value, fromNativeBS) where

import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE
import System.OsPath
import System.IO.Unsafe ( unsafePerformIO )
import Control.DeepSeq (force)
import Data.ByteString (ByteString)
import Data.Kind (Constraint, Type)

import ValueCompat

{-# NOINLINE encodeVal #-}
encodeVal :: String -> OsPath
encodeVal = force . unsafePerformIO . encodeFS

class Show a => Value a where
    toCmdLine :: a -> [OsString]
    toSignature :: a -> [ByteString]
    toSignature v = toNativeBS <$> toCmdLine v

instance Value () where
    toCmdLine _ = []
    toSignature _ = []

instance Value Int where
    toCmdLine = (:[]) . encodeVal . show

instance Value TS.ShortText where
    toCmdLine = (:[]) . encodeVal . TS.unpack

instance Value String where
    toCmdLine = (:[]) . encodeVal

instance Value a => Value (Maybe a) where
    toCmdLine (Just a) = toCmdLine a
    toCmdLine Nothing = []

instance {-# OVERLAPPABLE #-} (Value a, Foldable f, Show (f a)) => Value (f a) where
    toCmdLine = foldMap toCmdLine

type ValueList f a = (Foldable f, Value (f a))
