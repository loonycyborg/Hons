{-# LANGUAGE BlockArguments #-}
module Options where

import Options.Applicative
import Text.Read (readEither)
import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bool (bool)
import Data.Char (isAlphaNum)
import Text.ParserCombinators.ReadP ( readP_to_S, char, munch1, eof )

import Taskmaster

data InvocOpts = InvocOpts {
    file :: String,
    chdir :: Maybe String,
    cmdlineTargets :: [Either String (String, String)]
}

invocOpts = InvocOpts <$>
    option str (
            long "file" <>
            short 'f' <>
            metavar "FILE" <>
            help "Use FILE as Honstruct file" <>
            value "Honstruct" <>
            showDefault
    ) <*>
    optional (option str $
        long "directory" <>
        short 'C' <>
        metavar "DIR" <>
        help "Change to directory DIR before doing anything"
    ) <*>
    many (argument (maybeReader parseVar) $
        metavar "TARGET | VARIABLE=VALUE"
    )

taskmasterOpts = TaskmasterSettings <$>
        lastOfMany nConc (option auto $
            long "jobs" <>
            short 'j' <>
            metavar "JOBS" <>
            help "Maximum number of jobs to run in parallel. 0 means infinite"
        ) <*>
        switch (
            long "always-build" <>
            short 'B' <>
            help "Unconditionally rebuild all targets"
        ) <*>
        switch (
            long "keep-going" <>
            short 'k' <>
            help "Even after a failure continue to build other targets that don't depend on failed targets"
        )

allOpts = (,) <$> invocOpts <*> taskmasterOpts

opts = info (allOpts <**> helper)
        (  fullDesc
        <> header "Hons - build automation tool")

lastOfMany :: Alternative f => a -> f a -> f a
lastOfMany def x = (last <$> some x) <|> pure def

parseVar :: String -> Maybe (Either String (String, String))
parseVar input = case readP_to_S argument input of
    [(result, "")] -> Just result
    _              -> Nothing
  where
  var = (,) <$> (var_name <* char '=') <*> munch1 (const True)
  var_name = munch1 ((||) <$> isAlphaNum <*> (=='.'))
  target = munch1 (/='=')
  argument = (Right <$> var) <|> (Left <$> target) <* eof

nConc :: Int
{-# NOINLINE nConc #-}
nConc = unsafePerformIO getNumProcessors
