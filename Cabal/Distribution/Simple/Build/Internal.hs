-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build.Internal
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
    generate  
  , ghc_newer_than
  ) where

import Prelude ()
import Data.List (intercalate)
import Distribution.Compat.Prelude

import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Text
import Distribution.Version

generate :: Text a
          => LocalBuildInfo
          -> a -- ^ Module name
          -> [String] -- ^ Pragmas 
          -> [String] -- ^ Exports
          -> [String] -- ^ Imports
          -> String   -- ^ Body
          -> String
generate lbi moduleName userPragmas userExports userImports body =
   let pragmas =
            no_rebindable_syntax_pragma
         ++ intercalate "\n" userPragmas
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

    in pragmas++
       "module " ++ display moduleName ++ " (\n"++ (intercalate "," userExports) ++ "  ) where\n" ++
       "\n"++
       (intercalate "\n" userImports) ++
       "\nimport Prelude\n"++
       "\n"++ body
  where
    supports_rebindable_syntax= ghc_newer_than lbi (mkVersion [7,0,1])

ghc_newer_than :: LocalBuildInfo -> Version -> Bool
ghc_newer_than lbi minVersion =
  case compilerCompatVersion GHC (compiler lbi) of
    Nothing -> False
    Just version -> version `withinRange` orLaterVersion minVersion
