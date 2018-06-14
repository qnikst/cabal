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
module Distribution.Simple.Build.VersionModule (
    generate, imports, body, exports
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.System
import Distribution.Simple.Compiler
import qualified Distribution.Simple.Build.Internal as Internal
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Version

import System.FilePath ( pathSeparator )

-- | Generate Version module.
generate :: LocalBuildInfo -> PackageDescription -> String
generate lbi pkg_descr =
    Internal.generate lbi moduleName [] exports imports (body pkg_descr)
  where
    moduleName = autogenVersionModuleName pkg_descr


-- | List of required imports.
imports :: [String]
imports = ["import Data.Version (Version(..))"]

-- | Body of the module
body :: PackageDescription -> String
body pkg_descr = "version :: Version\n"
                 ++ "version = Version " ++ show branch ++ " []"
  where branch = versionNumbers $ packageVersion pkg_descr

exports :: [String]
exports = ["version"]
