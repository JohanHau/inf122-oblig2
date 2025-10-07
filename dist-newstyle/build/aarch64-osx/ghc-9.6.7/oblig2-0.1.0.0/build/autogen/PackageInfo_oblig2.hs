{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_oblig2 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "oblig2"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "INF122 Oblig 2 - Regneark (Spreadsheet)"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
