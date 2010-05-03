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

module Commands.Vulns
    ( vulnUpdate
    , vulnList
    , vulnReport
    ) where

import Control.Monad

import qualified AlertDB as ADB
import qualified Types as T
import qualified LWN
import qualified Report as R
import qualified Commands.ArgTypes as CAT
import qualified Filter as F

-- {{{1 vulnUpdate
vulnUpdate (CAT.VulnUpdate dbPath _) = let
        keepAlert alertIds alert = T.alertIdentity alert `notElem` alertIds
    in do
        distros <- ADB.runAlertDB dbPath ADB.getAllDistro
        alerts <- liftM concat $ mapM LWN.getAlerts distros
        alertIds <- ADB.runAlertDB dbPath ADB.getAllAlertId
        let filteredAlerts = filter (keepAlert alertIds) alerts
        ADB.runAlertDB dbPath (mapM_ ADB.addAlert filteredAlerts)

vulnUpdate _ = error "vulnUpdate called with bad argument"

-- {{{1 vulnList
vulnList :: CAT.ArgType -> IO ()
vulnList (CAT.VulnList dbPath ty size nf) = let
        getAlerts = case ty of
            CAT.Unreported -> ADB.getUnreportedAlert
            CAT.Reported   -> ADB.getReportedAlert
            CAT.All        -> ADB.getAllAlert
        filter = if nf
            then return
            else F.filterAlerts
        prettyPrinter = case size of
            CAT.Small -> T.prettyTerse
            CAT.Normal -> T.prettyNormal
            CAT.Full -> T.prettyVerbose
    in
        ADB.runAlertDB dbPath getAlerts >>= filter >>= mapM_ (print . prettyPrinter)

vulnList _ = error "vulnList called with bad argument"

-- {{{1 vulnReport
vulnReport :: CAT.ArgType -> IO ()
vulnReport (CAT.VulnReport dbPath nf) = let
        filter = if nf
            then return
            else F.filterAlerts
    in ADB.runAlertDB dbPath ADB.getUnreportedAlert >>= filter >>= R.report dbPath

vulnReport _ = error "vulnReport called with bad argument"
