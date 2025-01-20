Hons: SCons-like build tool implemented in Haskell
==================================================

SYNOPSIS
--------
The aim is to implement a build tool with dsl/api inspired by
SCons' SConstruct files but typesafe and more declarative

PREREQUISITES
-------------
Haskell ecosystem, either from your distro or GHCup

RUNNING
-------
`cabal run hons`

this will run example script in app/Main.hs
which will in turn generate graph.dot file with DAG the build would have
