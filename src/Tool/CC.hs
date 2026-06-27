{-# LANGUAGE BlockArguments, DataKinds, TemplateHaskell, ImplicitParams, OverloadedStrings, OverloadedLists, LambdaCase, PatternSynonyms, ViewPatterns, QuasiQuotes, TypeAbstractions #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Tool.CC where

import CmdLine
import Environment
import Node
import DepGraph
import Data.Tagged
import Text.ParserCombinators.ReadP
import Data.Makefile
import Data.Makefile.Parse (parseMakefileContents)
import Data.Char
import Data.String (fromString)
import Data.List (uncons)
import Data.Maybe (mapMaybe)
import Data.Foldable
import System.OsPath ( (-<.>), osp, isExtensionOf )
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Hashable (hash)
import GHC.Exts (IsString)

import ToolTH
import Builder (propagateIO, Builder (PropagateIO, Builder), ChainType(BuilderC,PropagatorC), depends, evaluate, emptyRuleSet)
import Action ( EvalResult(EvalResult, ResultFailure), modenv, ActionEval, Task, noResult, ActionSig )

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
instance IsString Lib where
  fromString = LibSpec . fromString

data IncludeDir = Include { iname :: StrVar } | SystemInclude { iname :: StrVar } | IncludeAfter { iname :: StrVar } deriving (Show, Read, Eq)
instance ConstructionVariable IncludeDir where
  merge = exclusiveMerge
  fromCmdLine =    (SystemInclude . fromString <$> (string         "sysinc:" *> munch (const True)))
               +++ ( IncludeAfter . fromString <$> (string       "incafter:" *> munch (const True)))
               <++ (      Include . fromString <$> (optional (string "inc:") *> munch (const True)))
instance Value IncludeDir where
  toCmdLine incl = toCmdLine incl.iname
instance IsString IncludeDir where
  fromString = Include . fromString

data CPPDefine = CPPDefine StrVar | CPPDefineWithValue StrVar StrVar deriving (Show, Read, Eq)
instance ConstructionVariable CPPDefine where
  merge = exclusiveMerge
  fromCmdLine = (CPPDefineWithValue . fromString <$> munch (/='=') <*> (char '=' *> (fromString <$> munch (const True))))
            <++ (CPPDefine . fromString <$> munch (const True))
instance Value CPPDefine where
  toCmdLine (CPPDefine d) = toCmdLine d
  toCmdLine (CPPDefineWithValue d val) = toCmdLine $ d <> "=" <> val
instance IsString CPPDefine where
  fromString = CPPDefine . fromString

$genToolVars

data Flag where
    Literal    :: Value a => a -> Flag
    Compile    :: Flag
    Preprocess :: Flag
    Deps       :: Flag
    SysDeps    :: Flag
    Output     :: Value a => a -> Flag
    CPPPath    :: (ValueList f IncludeDir) => f IncludeDir -> Flag
    CPPDefines :: (ValueList f CPPDefine) => f CPPDefine -> Flag
    LibPath    :: Value a => a -> Flag
    Libs       :: (ValueList f Lib) => f Lib -> Flag

deriving instance Show Flag

instance Value Flag where
    toCmdLine (Literal as) = toCmdLine as
    toCmdLine Compile = [encodeVal "-c"]
    toCmdLine Preprocess = [encodeVal "-E"]
    toCmdLine Deps = [encodeVal "-MM"]
    toCmdLine SysDeps = [encodeVal "-M"]
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

genCFlags :: (UseEnv ToolVars vars, ?t::Task vars, ?e::Environment vars) => CmdLine -> CmdLine
genCFlags c = c :$ Literal cflags :$ CPPPath cpppath :$ CPPDefines cppdefines

compile :: UseEnv ToolVars vars => Node -> Node -> RuleSet vars
compile tgt src = c tgt src <> cscan tgt src where
  c = osCommand $ genCFlags $ Cmd cccom :$ Compile :$ Output substT :$ substS

cscan :: UseEnv ToolVars vars => Node -> Node -> RuleSet vars
cscan tgt src =
    let scan_name = nodePathString src <> ".cscan"
        val = mkValue scan_name
        scan_cmd :: (UseEnv ToolVars vars, ?t::Task vars, ?e::Environment vars) => CmdLine
        scan_cmd = genCFlags $ Cmd cccom :$ Preprocess :$ SysDeps :$ src
        do_scan = do
          scan_result <- osExecutePipeStdout scan_cmd
          makefile_text <- T.pack <$> maybe (fail "Scanner command failed") decodeFilename scan_result
          makefile <- either fail return $ parseMakefileContents makefile_text
          let deps = map dep2node $ concat $ mapMaybe extract_dep makefile.entries where
                extract_dep (Rule _ deps _) = Just deps
                extract_dep _               = Nothing
                dep2node (Dependency d) = mkFsNodeFromString $ T.unpack d
          return (EvalResult $ hash deps, deps)
    in
      depends tgt val <> evaluate val src do_scan (inTaskContext $ return $ toSignature scan_cmd)

link :: (UseEnv ToolVars vars, Value s, NodeList s) => Node -> s -> RuleSet vars
link = osCommand $ Cmd linkcom :$ Literal linkflags :$ LibPath libpath :$ Libs libs :$ Output substT :$ substS

data ObjectBuilder = ObjectBuilder (forall t . UseEnv ToolVars t => Node -> Node -> RuleSet t) | LiteralObject

program :: UseEnv ToolVars vars => StrVar -> [(StrVar, ObjectBuilder)] -> RuleSet vars
program @vars (StrVar name) = link_objects . foldMap compile_object where
  compile_object :: (StrVar, ObjectBuilder) -> ([Node], RuleSet vars)
  compile_object (StrVar name, ObjectBuilder func) = ([tgt], (func @vars) tgt src) where [ tgt, src ] = map FsNode [ name -<.> [osp|.o|], name ]
  compile_object (StrVar name, LiteralObject)      = ([FsNode name], emptyRuleSet)
  link_objects (objects, sg) = sg <> link (FsNode name) objects

pattern Program :: UseEnv ToolVars vars => StrVar -> NonEmpty (Builder vars BuilderC) -> Builder vars BuilderC
pattern Program <- (const False -> True) where
  Program tgt src = Builder program program_node source_builders tgt src where
    program_node (StrVar name) = NE.singleton $ FsNode name
    source_builders            = NE.toList . fmap source_builder
    source_builder (FsNode name)
      | [osp|.c|] `isExtensionOf` name = (StrVar name, ObjectBuilder compile)
      | otherwise    = (StrVar name, LiteralObject)
    source_builder _ = error "Value nodes are not supported as program sources"

pkgConfig :: (UseEnv ToolVars vars) => String -> ActionEval vars
pkgConfig p = do
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
      eInsertTS CFLAGS newcflags
    . eInsertTS CPPDEFINES newdefines
    . eInsertTS CPPPATH newpath
  let Just parsedlibs = parseFlags $ toString $ StrVar pkglibs
  let (newlinkflags, newlibs, newlibpath) = foldr (\cases
          (LibPath p) (fs, ls, ps) -> (fs, ls, map StrVar (toCmdLine p) <> ps)
          (Libs l)    (fs, ls, ps) -> (fs, Data.Foldable.toList l <> ls, ps)
          flag        (fs, ls, ps) -> (map StrVar (toCmdLine flag) <> ps, ls, ps)
        ) ([], [], []) parsedlibs
  modenv $
      eInsertTS LINKFLAGS newlinkflags
    . eInsertTS LIBS newlibs
    . eInsertTS LIBPATH newlibpath
  return (EvalResult $ StrVar version, [])

pattern Pkg :: (UseEnv ToolVars vars) => String -> Builder vars PropagatorC
pattern Pkg <- (const False -> True) where
  Pkg name = PropagateIO (pkgConfig name) (name ++ "-pkg-config")
