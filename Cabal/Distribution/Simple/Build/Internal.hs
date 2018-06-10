-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build.Macros
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generating the Paths_pkgname module.
--
-- This is a module that Cabal generates for the benefit of packages. It
-- enables them to find their version number and find any installed data files
-- at runtime. This code should probably be split off into another module.
--
module Distribution.Simple.Build.Internal (
    generate, pkgPathEnvVar
  ) where

generateHeader moduleName otherPragmas body =
   let pragmas =
            cpp_pragma
         ++ no_rebindable_syntax_pragma
         ++ otherPragmas
         ++ warning_pragmas

       -- -XRebindableSyntax is problematic because when paired with
       -- -XOverloadedLists, 'fromListN' is not in scope,
       -- or -XOverloadedStrings 'fromString' is not in scope,
       -- so we disable 'RebindableSyntax'.
       no_rebindable_syntax_pragma
         | supports_rebindable_syntax = "{-# LANGUAGE NoRebindableSyntax #-}\n"
         | otherwise                  = ""

       warning_pragmas =
        "{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}\n"

       warning_pragmas =
        "{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}\n"

       header =
         pragmas++
         "module " ++ display moduleName ++ " (\n"++ exports ++ ") where\n" ++
         "\n"++
         otherImports
         "import Prelude\n"++
         "\n"++ body
