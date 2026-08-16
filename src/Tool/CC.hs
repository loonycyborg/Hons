{-# LANGUAGE BlockArguments, DataKinds, TemplateHaskell, ImplicitParams, OverloadedStrings, OverloadedLists, LambdaCase, QuasiQuotes, TypeAbstractions,
  DeriveAnyClass, NoGeneralizedNewtypeDeriving #-}
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
import Data.Bool
import Data.String (fromString)
import Data.List (uncons, stripPrefix)
import Data.Maybe (mapMaybe, isJust, fromJust, maybeToList, fromMaybe, isNothing)
import Data.Foldable
import Control.Applicative ((<|>))
import Control.Monad
import Control.Arrow ((&&&))
import System.OsPath ( (-<.>), osp, isExtensionOf, OsPath, takeDirectory )
import qualified System.FilePath
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)
import Data.Hashable (hash)
import Data.Aeson (FromJSON (..), genericParseJSON, Options (..), defaultOptions, camelTo2, eitherDecodeFileStrict)
import GHC.Exts (IsString)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Quote (QuasiQuoter)

import ToolTH
import Builder (propagateIO, Builder (PropagateIO, Builder), ChainType(BuilderC,PropagatorC), depends, evaluate, emptyRuleSet, propagate, Tag (..), tag, TagRegistry, sourcelistT)
import Action ( EvalResult(EvalResult, ResultFailure), modenv, ActionEval, Task, noResult, ActionSig, getenv, liftIO )


toolEnv =
  envVar @("cc" :. "cccom")     ("gcc" :: StrVar)         :+:
  envVar @("cc" :. "cxxcom")    ("g++" :: StrVar)         :+:
  envVar @("cc" :. "cflags")    ([] :: TSList StrVar)     :+:
  envVar @("cc" :. "cstd")      (Nothing :: Maybe CStd)   :+:
  envVar @("cc" :. "cxxstd")    (Nothing :: Maybe CXXStd) :+:
  envVar @("cc" :. "cpppath")   ([] :: TSList IncludeDir) :+:
  envVar @("cc" :. "cppdefines") ([] :: TSList CPPDefine) :+:
  envVar @("cc" :. "linkcom")   (CLinker :: Linker)       :+:
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

data CStd = C90 | C99 | C11 | C17 | C23 deriving (Show, Read, Eq, Ord)
instance ConstructionVariable CStd where
  merge = const

data CXXStd = CXX98 | CXX03 | CXX11 | CXX17 | CXX20 | CXX23 | CXX26 deriving (Show, Read, Eq, Ord)
instance ConstructionVariable CXXStd where
  merge = const
cxxStdYear :: CXXStd -> [Char]
cxxStdYear = fromJust . stripPrefix "CXX" . show

data Linker = CLinker | CXXLinker deriving (Show, Read, Eq, Ord)
instance Semigroup Linker where
  (<>) = max
instance ConstructionVariable Linker where
  merge = max
instance Monoid Linker where
  mempty = CLinker

$genToolVars

data Flag where
    Literal    :: Value a => a -> Flag
    Compile    :: Flag
    Preprocess :: Flag
    Deps       :: Flag
    SysDeps    :: Flag
    Std        :: CStd -> Flag
    StdXX      :: CXXStd -> Flag
    CXXModules :: Flag
    CXXScan    :: (Value a, Value b) => a -> b -> Flag
    HeaderSrc  :: Value a => a -> Flag
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
    toCmdLine (Std std) = [encodeVal $ "-std=" <> map toLower (show std)]
    toCmdLine (StdXX std) = [encodeVal $ "-std=c++" <> cxxStdYear std]
    toCmdLine CXXModules = [encodeVal "-fmodules"]
    toCmdLine (CXXScan json obj) = [encodeVal "-fdeps-format=p1689r5"]
                           <> ((encodeVal "-fdeps-file="<>)   <$> toCmdLine json)
                           <> ((encodeVal "-fdeps-target="<>) <$> toCmdLine obj)
    toCmdLine (HeaderSrc a) = encodeVal "-fsearch-include-path" : toCmdLine a
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
    Std . read_literal <$> (string "-std=" *> literal),
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
    read_literal = read . map toUpper . toString

flagsP :: ReadP [Flag]
flagsP = skipSpaces *> sepBy flagP (munch1 isSpace) <* skipSpaces <* eof

parseFlags :: String -> Maybe [Flag]
parseFlags = fmap (fst . fst) . uncons . readP_to_S flagsP

genCFlags :: (UseEnv ToolVars vars, ?t::Task vars, ?e::Environment vars) => CmdLine
genCFlags = Cmd cccom :$ (Std <$> cstd) :$ Literal cflags :$ CPPPath cpppath :$ CPPDefines cppdefines

genCXXFlags :: IsProgramSource t => t -> (UseEnv ToolVars vars, ?t::Task vars, ?e::Environment vars) => CmdLine
genCXXFlags t = Cmd cxxcom :$ (StdXX <$> cxxstd) :$ bool (Literal ()) CXXModules (isJust $ modulesEnabled t) :$ Literal cflags :$ CPPPath cpppath :$ CPPDefines cppdefines

compile :: (UseEnv ToolVars vars, IsProgramSource t) => t -> Node -> Node -> RuleSet vars
compile t = osCommand $ genFlags t :$ Compile :$ bool (Literal ()) (Output substT) (isNothing (modulesEnabled t) || (modulesEnabled t == Just ModSource)) :$ map expandSrcNode substS

expandSrcNode :: Node -> Flag
expandSrcNode n@(FsNode {}) = Literal n
expandSrcNode (ValueNode n _) = HeaderSrc $ parse_header_module n where
  parse_header_module n = case stripPrefix "cxx-module-src-" n of
    Just "std"        -> "bits/std.cc" :: String
    Just "std.compat" -> "bits/std.compat.cc"
    _ -> error $ "Unknown c++ module specification: " <> n

jsopts :: Options
jsopts = defaultOptions  { fieldLabelModifier = camelTo2 '-' }

data P1689r5Module = P1689r5Module { logicalName :: String, compiledModulePath :: Maybe String } deriving (Show, Generic)
instance FromJSON P1689r5Module where parseJSON = genericParseJSON jsopts
data P1689r5Rule = P1689r5Rule { primaryOutput :: String, provides :: Maybe [P1689r5Module], requires :: Maybe [P1689r5Module] } deriving (Show, Generic)
instance FromJSON P1689r5Rule where parseJSON = genericParseJSON jsopts
newtype P1689r5DepsFile = P1689r5DepsFile { rules :: [P1689r5Rule] } deriving (Show, Generic, FromJSON)

cscan :: (UseEnv ToolVars vars, IsProgramSource t) => t -> Node -> Node -> (Maybe Node, RuleSet vars)
cscan t tgt src =
    let src_name = case src of
          FsNode {} -> nodePathString src
          ValueNode n _ -> n
        module_deps_name = bool Nothing (Just $ mkFsNodeFromString $ src_name <> ".json") (isJust $ modulesEnabled t)
        val = mkValue $ src_name <> ".cscan"
        scan_cmd :: (UseEnv ToolVars vars, ?t::Task vars, ?e::Environment vars) => CmdLine
        scan_cmd = genFlags t :$ Preprocess :$ SysDeps :$
          bool (Literal ()) (CXXScan module_deps_name tgt) (isJust $ modulesEnabled t) :$
          expandSrcNode src
        do_scan = do
          scan_result <- osExecutePipeStdout scan_cmd
          env <- getenv
          makefile_text <- T.pack <$> maybe (fail "Scanner command failed") decodeFilename scan_result
          makefile <- either fail return $ parseMakefileContents makefile_text
          let deps = map dep2node $ concat $ mapMaybe extract_dep makefile.entries where
                extract_dep (Rule _ deps _) = Just deps
                extract_dep _               = Nothing
                dep2node (Dependency d) = mkFsNodeFromString $ T.unpack d
          return (EvalResult $ hash deps, map (tgt,) deps)
    in
      (module_deps_name, depends tgt val <> depends tgt module_deps_name <> evaluate (fromMaybe val module_deps_name) src do_scan (inTaskContext $ return $ toSignature scan_cmd))

cxxmodulescan :: Node -> [Node] -> RuleSet vars
cxxmodulescan tgt srcs = evaluate tgt srcs do_scan (return []) where
  do_scan = do
    let jsons = map nodePathString srcs
    deps :: [P1689r5DepsFile] <- map (either error id) <$> mapM (liftIO . eitherDecodeFileStrict) jsons
    let module_map = HM.fromList $ concatMap (mapMaybe (module_entry . (((fmap ((.logicalName) . fst) . uncons) <=< (.provides)) &&& (.primaryOutput))) . ((.rules))) deps
        module_entry (Just name, object) = Just (name, object)
        module_entry _                   = Nothing
        dependencies = concatMap (map ((.primaryOutput) &&& (maybe [] (fmap (.logicalName)) . ((.requires)))) . (.rules)) deps
        imps = concatMap (\(tgt, imports) -> map ((tgt,) . lookup_module) imports) dependencies
        lookup_module m = case HM.lookup m module_map of
          Just f -> Right f
          Nothing -> Left m
        resolved_imps = map resolve_imp imps
        resolve_imp (t, Right file) = (mkFsNodeFromString t, mkFsNodeFromString file)
        resolve_imp (t, Left mod) = (mkFsNodeFromString t, mkFsNodeFromString $ "gcm.cache/" <> mod <> ".gcm")
    return (noResult, resolved_imps)

link :: (UseEnv ToolVars vars, Value s, NodeList s) => Linker -> Node -> s -> RuleSet vars
link @vars linker = osCommand $ Cmd ld :$ Literal linkflags :$ LibPath libpath :$ Libs libs :$ Output substT :$ substS where
  ld :: (?e::Environment vars) => StrVar
  ld = case linker of
    CLinker   -> cccom
    CXXLinker -> cxxcom

program :: UseEnv ToolVars vars => StrVar -> NonEmpty (Builder vars BuilderC) -> Builder vars BuilderC
program = Builder TagNihil program_builder program_node source_builders where
    program_node (StrVar name) = NE.singleton $ FsNode name
    source_builders :: (?tags::TagRegistry) => NonEmpty Node -> [(StrVar, Maybe SourceT)]
    source_builders            = NE.toList . fmap source_builder
    source_builder n@(FsNode name) = (StrVar name, tag @SourceT n <|> autoTag name)
    source_builder _               = error "Value nodes are not supported as program sources"
    program_builder :: UseEnv ToolVars vars => StrVar -> [(StrVar, Maybe SourceT)] -> RuleSet vars
    program_builder @vars (StrVar name) sources = link_objects . foldMap compile_object $ sources where
      compile_object :: (StrVar, Maybe SourceT) -> ([Node], RuleSet vars, [Node], Linker)
      compile_object (StrVar name, Just (SourceT t)) = ([tgt], objectCompiler t tgt src <> scan, maybeToList json, objectLinker t) where
            [ tgt, src ] = map FsNode [ name -<.> [osp|.o|], name ]
            (json, scan) = objectScanner t tgt src
      compile_object (StrVar name, Nothing)          = ([FsNode name], emptyRuleSet, [], CLinker)
      cxx_std_module name =
        objectCompiler CXXModuleHeader mod mod_src <>
        snd (objectScanner CXXModuleHeader mod mod_src) <>
        depends mod_src (FsNode . (.unStrVar) . fst . fst <$> uncons sources) where
          mod = mkFsNodeFromString $ "gcm.cache/" <> name <> ".gcm"
          mod_src = mkValue $ "cxx-module-src-" <> name
      link_objects (objects, sg, jsons, linker) =
        sg <>
        (if not $ null jsons then
          cxxmodulescan (mkValue "cxx-module-mapper") jsons <>
          depends objects (mkValue "cxx-module-mapper") <>
          cxx_std_module "std" <>
          cxx_std_module "std.compat"
        else emptyRuleSet) <>
        link linker (FsNode name) objects

data ModuleBuildMode = ModSource | ModHeader deriving (Eq, Show)

class IsProgramSource a where
  extensions :: a -> [OsPath]
  extensions a = []
  modulesEnabled :: a -> Maybe ModuleBuildMode
  modulesEnabled a = Nothing
  genFlags       :: a -> (forall vars . (UseEnv ToolVars vars, ?t::Task vars, ?e::Environment vars) => CmdLine)
  objectCompiler :: a -> (UseEnv ToolVars vars => Node -> Node -> RuleSet vars)
  objectScanner  :: a -> (UseEnv ToolVars vars => Node -> Node -> (Maybe Node, RuleSet vars))
  objectLinker   :: a -> Linker

data SourceT = forall a . (IsProgramSource a, Show a, Lift a) => SourceT { filetype :: a }
deriving instance Show SourceT
deriving instance Lift SourceT

data C = C deriving (Show, Lift)
instance IsProgramSource C where
  extensions _ = [[osp|.c|]]
  genFlags t = genCFlags
  objectCompiler t = compile t
  objectScanner t  = cscan t
  objectLinker _ = CLinker

data CXX = CXX | CXXModuleSrc | CXXModuleHeader deriving (Show, Lift)
instance IsProgramSource CXX where
  extensions _ = [[osp|.cpp|], [osp|.cc|], [osp|.cxx|], [osp|.C|]]
  modulesEnabled CXX             = Nothing
  modulesEnabled CXXModuleSrc    = Just ModSource
  modulesEnabled CXXModuleHeader = Just ModHeader
  genFlags t = genCXXFlags t
  objectCompiler t = compile t
  objectScanner t  = cscan t
  objectLinker _ = CXXLinker

autoTag :: OsPath -> Maybe SourceT
autoTag path = find @NonEmpty matches [SourceT C, SourceT CXX] where
  matches (SourceT filetype) = isJust $ find (`isExtensionOf` path) (extensions filetype)

sourcelistMod :: QuasiQuoter
sourcelistMod = sourcelistT $ SourceT CXXModuleSrc :# TagNihil

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

pkg :: (UseEnv ToolVars vars) => String -> Builder vars PropagatorC
pkg name = PropagateIO (pkgConfig name) (name ++ "-pkg-config")
