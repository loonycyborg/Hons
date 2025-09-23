{-# LANGUAGE BlockArguments #-}
module Options where

import Options.Applicative
import Text.Read (readEither)
import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafePerformIO)

import Taskmaster

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

opts = info (taskmasterOpts <**> helper)
        (  fullDesc
        <> header "Hons - build automation tool")

lastOfMany :: Alternative f => a -> f a -> f a
lastOfMany def x = (last <$> some x) <|> pure def

nConc :: Int
{-# NOINLINE nConc #-}
nConc = unsafePerformIO getNumProcessors
