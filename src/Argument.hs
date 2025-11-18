module Argument where

import qualified Data.Text.Short as TS
import qualified Data.List.NonEmpty as NE
import System.OsPath
import System.IO.Unsafe ( unsafePerformIO )

import Node
import Environment

{-# NOINLINE encodeArg #-}
encodeArg = unsafePerformIO . encodeFS

class Argument a where
    toCmdLine :: a -> [OsString]

data NoArg = NoArg deriving (Show, Eq)
instance Argument NoArg where
    toCmdLine _ = [ encodeArg "!!!NoArg!!!" ]

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
    toCmdLine (ValueNode v) = [encodeArg v]

instance {-# OVERLAPPABLE #-} (Argument a, Foldable f) => Argument (f a) where
    toCmdLine = foldMap toCmdLine
