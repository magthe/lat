{-# LANGUAGE DeriveDataTypeable #-}

{- lat - tool to track alerts from LWN.
 - Copyright (C) 2010  Magnus Therning
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, version 3 of the License.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

module Commands.ArgTypes
    where

import Data.Data

data VulnListType = All | Reported | Unreported
    deriving (Data, Typeable, Show, Eq)

data VulnListSize = Small | Normal | Full
    deriving (Data, Typeable, Show, Eq)

data ArgType
    = DistroAdd { config :: FilePath , name :: String , url :: String }
    | DistroList { config :: FilePath }
    | VulnUpdate { config :: FilePath, dry :: Bool }
    | VulnList
        { config :: FilePath
        , typ :: VulnListType
        , size :: VulnListSize
        , nofilter :: Bool
        }
    | VulnReport { config :: FilePath, nofilter :: Bool }
    deriving (Data, Typeable, Show)
