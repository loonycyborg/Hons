{-# LANGUAGE BlockArguments, DataKinds, TemplateHaskell, ImplicitParams, OverloadedStrings, OverloadedLists #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Tool.CC where

import CmdLine
import Environment
import Node
import DepGraph
import Data.Tagged
import Text.ParserCombinators.ReadP
import Data.Char
import Data.String (fromString)
import Data.List (unsnoc)

import ToolTH

toolEnv =
  envVar @("cc" :. "cccom")     ("gcc" :: StrVar)         :+:
  envVar @("cc" :. "cflags")    ([] :: TSList StrVar)     :+:
  envVar @("cc" :. "cpppath")   ([] :: TSList IncludeDir) :+:
  envVar @("cc" :. "cppdefines") ([] :: TSList CPPDefine) :+:
  envVar @("cc" :. "linkcom")   ("gcc" :: StrVar)         :+:
  envVar @("cc" :. "linkflags") ([] :: TSList StrVar)     :+:
  envVar @("cc" :. "libpath")   ([] :: TSList StrVar)     :+:
  envVar @("cc" :. "libs")      ([] :: TSList Lib)        :+:
  EnvNihil

data Lib = LibSpec { lname :: StrVar } | LibFile { lname :: StrVar } deriving (Show, Read, Eq)
instance ConstructionVariable Lib where
  merge = exclusiveMerge
instance Value Lib where
  toCmdLine lib = toCmdLine lib.lname

data IncludeDir = Include { iname :: StrVar } | SystemInclude { iname :: StrVar } | IncludeAfter { iname :: StrVar } deriving (Show, Read, Eq)
instance ConstructionVariable IncludeDir where
  merge = exclusiveMerge
instance Value IncludeDir where
  toCmdLine incl = toCmdLine incl.iname

data CPPDefine = CPPDefine StrVar | CPPDefineWithValue StrVar StrVar deriving (Show, Read, Eq)
instance ConstructionVariable CPPDefine where
  merge = exclusiveMerge
instance Value CPPDefine where
  toCmdLine (CPPDefine d) = toCmdLine d
  toCmdLine (CPPDefineWithValue d val) = toCmdLine $ d <> "=" <> val

$genToolVars

data Flag where
    Literal    :: Value a => a -> Flag
    Compile    :: Flag
    Output     :: Value a => a -> Flag
    CPPPath    :: (Foldable f, Value (f IncludeDir)) => f IncludeDir -> Flag
    CPPDefines :: (Foldable f, Value (f CPPDefine)) => f CPPDefine -> Flag
    LibPath    :: Value a => a -> Flag
    Libs       :: (Foldable f, Value (f Lib)) => f Lib -> Flag

deriving instance Show Flag

instance Value Flag where
    toCmdLine (Literal as) = toCmdLine as
    toCmdLine Compile = [encodeVal "-c"]
    toCmdLine (Output a) = encodeVal "-o" : toCmdLine a
    toCmdLine (CPPPath as) = concatMap cppflag as where
      cppflag x = case x of
        Include p       -> (encodeVal "-I"<>)          <$> toCmdLine p
        SystemInclude p -> (encodeVal "-isystem="<>)   <$> toCmdLine p
        IncludeAfter p  -> (encodeVal "-idirafter="<>) <$> toCmdLine p
    toCmdLine (CPPDefines as) = (encodeVal "-D"<>) <$> toCmdLine as
    toCmdLine (LibPath as) = (encodeVal "-L"<>) <$> toCmdLine as
    toCmdLine (Libs as) = concatMap libflag as where
      libflag x = case x of
        LibSpec l -> (encodeVal "-l"<>) <$> toCmdLine l
        LibFile l -> toCmdLine l

flagP :: ReadP Flag
flagP = choice [
    Compile <$ string "-c",
    Output <$> (string "-o" *> literal),
    CPPPath <$> sepBy1 (
      (Include <$> (string "-I" *> literal)) +++
      (SystemInclude <$> (string "-isystem=" *> literal)) +++
      (IncludeAfter <$> (string "-idirafter=" *> literal))
    ) skipSpaces,
    CPPDefines <$> sepBy1 (
      (CPPDefine <$> (string "-D" *> literal)) +++
      (CPPDefineWithValue <$> (string "-D" *> literal) <*> (char '=' *> literal))
    ) skipSpaces,
    LibPath <$> sepBy1 (string "-L" *> literal) skipSpaces,
    Libs <$> sepBy1 (LibSpec <$> (string "-l" *> literal)) skipSpaces
  ] <++
    (Literal <$> literal)
  where
    literal = fromString . concat <$> many1 (escape <++ munch1 (\x -> not (isSpace x) && x /= '\\' && x /= '\''))
    escape_quote = char '\'' *> munch (/='\'') <* char '\''
    escape_backslash = char '\\' *> ((:[]) <$> get)
    escape = choice [escape_quote, escape_backslash]

flagsP :: ReadP [Flag]
flagsP = skipSpaces *> sepBy flagP (munch1 isSpace) <* skipSpaces <* eof

parseFlags :: String -> Maybe [Flag]
parseFlags = fmap (fst . snd) . unsnoc . readP_to_S flagsP

compile :: (UseEnv ToolVars vars) => Node -> Node -> RuleSet vars
compile = osCommand $ Cmd cccom :$ Literal cflags :$ CPPPath cpppath :$ CPPDefines cppdefines :$ Compile :$ Output substT :$ substS

link :: (UseEnv ToolVars vars, Value s, NodeList s) => Node -> s -> RuleSet vars
link = osCommand $ Cmd linkcom :$ Literal linkflags :$ LibPath libpath :$ Libs libs :$ Output substT :$ substS
