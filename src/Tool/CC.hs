{-# LANGUAGE BlockArguments, DataKinds, TemplateHaskell, ImplicitParams, OverloadedStrings, OverloadedLists, LambdaCase #-}
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
import Data.List (uncons)
import Data.Foldable

import ToolTH
import Builder (propagateIO)
import Action ( EvalResult(EvalResult), modenv )

toolEnv =
  envVar @("cc" :. "cccom")     ("gcc" :: StrVar)         :+:
  envVar @("cc" :. "cflags")    ([] :: TSList StrVar)     :+:
  envVar @("cc" :. "cpppath")   ([] :: TSList IncludeDir) :+:
  envVar @("cc" :. "cppdefines") ([] :: TSList CPPDefine) :+:
  envVar @("cc" :. "linkcom")   ("gcc" :: StrVar)         :+:
  envVar @("cc" :. "linkflags") ([] :: TSList StrVar)     :+:
  envVar @("cc" :. "libpath")   ([] :: TSList StrVar)     :+:
  envVar @("cc" :. "libs")      ([] :: TSList Lib)        :+:
  envVar @("cc" :. "pkgconfig") ("pkg-config" :: StrVar)  :+:
  EnvNihil

data Lib = LibSpec { lname :: StrVar } | LibFile { lname :: StrVar } deriving (Show, Read, Eq)
instance ConstructionVariable Lib where
  merge = exclusiveMerge
  fromCmdLine = (LibFile . fromString <$> (       string "libfile:" *> munch (const True)))
            <++ (LibSpec . fromString <$> (optional (string "lib:") *> munch (const True)))
instance Value Lib where
  toCmdLine lib = toCmdLine lib.lname

data IncludeDir = Include { iname :: StrVar } | SystemInclude { iname :: StrVar } | IncludeAfter { iname :: StrVar } deriving (Show, Read, Eq)
instance ConstructionVariable IncludeDir where
  merge = exclusiveMerge
  fromCmdLine =    (SystemInclude . fromString <$> (string         "sysinc:" *> munch (const True)))
               +++ ( IncludeAfter . fromString <$> (string       "incafter:" *> munch (const True)))
               <++ (      Include . fromString <$> (optional (string "inc:") *> munch (const True)))
instance Value IncludeDir where
  toCmdLine incl = toCmdLine incl.iname

data CPPDefine = CPPDefine StrVar | CPPDefineWithValue StrVar StrVar deriving (Show, Read, Eq)
instance ConstructionVariable CPPDefine where
  merge = exclusiveMerge
  fromCmdLine = (CPPDefineWithValue . fromString <$> munch (/='=') <*> (char '=' *> (fromString <$> munch (const True))))
            <++ (CPPDefine . fromString <$> munch (const True))
instance Value CPPDefine where
  toCmdLine (CPPDefine d) = toCmdLine d
  toCmdLine (CPPDefineWithValue d val) = toCmdLine $ d <> "=" <> val

$genToolVars

data Flag where
    Literal    :: Value a => a -> Flag
    Compile    :: Flag
    Output     :: Value a => a -> Flag
    CPPPath    :: (ValueList f IncludeDir) => f IncludeDir -> Flag
    CPPDefines :: (ValueList f CPPDefine) => f CPPDefine -> Flag
    LibPath    :: Value a => a -> Flag
    Libs       :: (ValueList f Lib) => f Lib -> Flag

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
    CPPPath . (:[]) <$> (
      (Include <$> (string "-I" *> literal)) +++
      (SystemInclude <$> (string "-isystem=" *> literal)) +++
      (IncludeAfter <$> (string "-idirafter=" *> literal))
    ),
    CPPDefines . (:[]) <$> (
      (CPPDefine <$> (string "-D" *> literal)) +++
      (CPPDefineWithValue <$> (string "-D" *> literal) <*> (char '=' *> literal))
    ),
    LibPath <$> (string "-L" *> literal),
    Libs . (:[]) . LibSpec <$> (string "-l" *> literal)
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
parseFlags = fmap (fst . fst) . uncons . readP_to_S flagsP

compile :: (UseEnv ToolVars vars) => Node -> Node -> RuleSet vars
compile = osCommand $ Cmd cccom :$ Literal cflags :$ CPPPath cpppath :$ CPPDefines cppdefines :$ Compile :$ Output substT :$ substS

link :: (UseEnv ToolVars vars, Value s, NodeList s) => Node -> s -> RuleSet vars
link = osCommand $ Cmd linkcom :$ Literal linkflags :$ LibPath libpath :$ Libs libs :$ Output substT :$ substS

pkg :: (UseEnv ToolVars vars, NodeList ns) => String -> ns -> RuleSet vars
pkg p src = propagateIO (p <> "-pkgconfig") src do
  Just version <-   osExecutePipeStdout $ Cmd pkgconfig :$ p :@ LogSilent :$ ("--modversion" :: StrVar)
  Just pkgcflags <- osExecutePipeStdout $ Cmd pkgconfig :$ p :@ LogSilent :$ ("--cflags" :: StrVar)
  Just pkglibs <-   osExecutePipeStdout $ Cmd pkgconfig :$ p :@ LogSilent :$ ("--libs" :: StrVar)
  let Just parsedc = parseFlags $ toString $ StrVar pkgcflags
  let (newcflags, newdefines, newpath) = foldr (\cases
          (CPPPath p)    (cs, ds, ps) -> (cs, ds, Data.Foldable.toList p <> ps)
          (CPPDefines d) (cs, ds, ps) -> (cs, Data.Foldable.toList d <> ds, ps)
          flag           (cs, ds, ps) -> (map StrVar (toCmdLine flag) <> cs, ds, ps)
        )
        ([], [], []) parsedc
  modenv $
      eInsertTS ("cc" :. "cflags") newcflags
    . eInsertTS ("cc" :. "cppdefines") newdefines
    . eInsertTS ("cc" :. "cpppath") newpath
  let Just parsedlibs = parseFlags $ toString $ StrVar pkglibs
  let (newlinkflags, newlibs, newlibpath) = foldr (\cases
          (LibPath p) (fs, ls, ps) -> (fs, ls, map StrVar (toCmdLine p) <> ps)
          (Libs l)    (fs, ls, ps) -> (fs, Data.Foldable.toList l <> ls, ps)
          flag        (fs, ls, ps) -> (map StrVar (toCmdLine flag) <> ps, ls, ps)
        ) ([], [], []) parsedlibs
  modenv $
      eInsertTS ("cc" :. "linkflags") newlinkflags
    . eInsertTS ("cc" :. "libs") newlibs
    . eInsertTS ("cc" :. "libpath") newlibpath
  return $ EvalResult $ StrVar version
