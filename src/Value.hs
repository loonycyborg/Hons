module Value (module Value, fromNativeBS) where

import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE
import System.OsPath
import System.IO.Unsafe ( unsafePerformIO )
import Data.ByteString (ByteString)

import ValueCompat
import Node
import Environment

{-# NOINLINE encodeVal #-}
encodeVal = unsafePerformIO . encodeFS

class Value a where
    toCmdLine :: a -> [OsString]
    toSignature :: a -> [ByteString]
    toSignature v = toNativeBS <$> toCmdLine v

data NoVal = NoVal deriving (Show, Eq)
instance Value NoVal where
    toCmdLine _ = [ encodeVal "!!!NoVal!!!" ]
    toSignature _ = []

instance Value Int where
    toCmdLine = (:[]) . encodeVal . show

instance Value TS.ShortText where
    toCmdLine = (:[]) . encodeVal . TS.unpack

instance Value StrVar where
    toCmdLine (StrVar a) = [a]

instance Value String where
    toCmdLine = (:[]) . encodeVal

instance Value a => Value (Maybe a) where
    toCmdLine (Just a) = toCmdLine a
    toCmdLine Nothing = []

instance Value Node where
    toCmdLine (FsNode f) = [f]
    toCmdLine (ValueNode v) = [encodeVal v]

instance {-# OVERLAPPABLE #-} (Value a, Foldable f) => Value (f a) where
    toCmdLine = foldMap toCmdLine
