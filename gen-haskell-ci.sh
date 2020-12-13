#!/bin/sh

# haskell-ci doesn't know how to regenerate multiple GHA workflows
haskell-ci github cabal.project
haskell-ci github --project cabal.deprecated.project -o .github/workflows/haskell-ci-deprecated.yml --config cabal.deprecated.haskell-ci
