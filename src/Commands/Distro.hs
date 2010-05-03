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

module Commands.Distro
    ( distroAdd
    , distroList
    ) where

import qualified AlertDB as ADB
import qualified Types as T
import qualified Commands.ArgTypes as CAT

-- {{{1 distroadd
distroAdd :: CAT.ArgType -> IO ()
distroAdd (CAT.DistroAdd dbPath name url) =
    ADB.runAlertDB dbPath (ADB.addDistro $ T.distro name (Just url))
distroAdd _ = print "Bad argument for distroAdd"

-- {{{1 distrolist
distroList :: CAT.ArgType -> IO ()
distroList (CAT.DistroList dbPath) =
    ADB.runAlertDB dbPath ADB.getAllDistro >>= mapM_ (print . T.prettyNormal)
distroList _ = print "Bad argument for distroList"
