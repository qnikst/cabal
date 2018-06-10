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
    generate, imports, body
  ) where

import Distribution.Simple.Build.Internal

-- | Generate Version module.
generate :: String
generate = Internal.generate version_modulename imports body

-- | List of required imports.
imports :: String
imports = "import Data.Version (Version(..))\n"

-- | Body of the module
body :: String 
body = "version :: Version"++
       "\nversion = Version " ++ show branch ++ " []"
  where branch = versionNumbers $ packageVersion pkg_descr
