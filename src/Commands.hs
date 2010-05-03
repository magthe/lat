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

module Commands
    ( doCommand
    ) where

import System.Console.CmdArgs
import System.Directory
import Data.Version (showVersion)
import Paths_lat (version)

import qualified Commands.Distro as D
import qualified Commands.Vulns as V
import qualified Commands.ArgTypes as CAT

-- {{{1 doCommand
doCommand = do
    home <- getAppUserDataDirectory "lat"
    let allArgs = map (\ f -> f home) [_distroAddArgs, _distroListArgs, _vulnUpdateArgs, _vulnListArgs, _vulnReportArgs]
    let ver = "lat " ++ (showVersion version) ++ " - Linux Alert Tracker"
            ++ "\nCopyright 2010 Magnus Therning <magnus@therning.org>"
    cmdArgs ver allArgs >>= _doCommand

_doCommand a@(CAT.DistroAdd {}) = D.distroAdd a
_doCommand a@(CAT.DistroList {}) = D.distroList a
_doCommand a@(CAT.VulnUpdate {}) = V.vulnUpdate a
_doCommand a@(CAT.VulnList {}) = V.vulnList a
_doCommand a@(CAT.VulnReport {}) = V.vulnReport a

-- {{{1 Command line arg definitions
_configAttrib = text "Set dir for configuration" & empty "~/.lat" & typDir

_distroAddArgs h = mode CAT.DistroAdd
    { CAT.config = h &= _configAttrib
    , CAT.name = def &= argPos 0 & typ "NAME"
    , CAT.url = def &= argPos 1 & typ "URL"
    } &= text "add a distribution"

_distroListArgs h = mode CAT.DistroList
    { CAT.config = h &= _configAttrib
    } &= text "list distributions"

_vulnUpdateArgs h = mode CAT.VulnUpdate
    { CAT.config = h &= _configAttrib
    , CAT.dry = def &= text "Dry run"
    } &= text "update the vulnerability database"

_vulnListArgs h = mode CAT.VulnList
    { CAT.config = h &= _configAttrib
    , CAT.typ = enum CAT.Unreported
        [ CAT.Unreported &= text "List unreported issues (default)"
        , CAT.Reported &= text "List reported issues"
        , CAT.All &= text "List all issues"
        ]
    , CAT.size = enum CAT.Normal
        [ CAT.Small &= text "Output format: small"
        , CAT.Normal &= text "Output format: normal" & flag "n"
        , CAT.Full &= text "Output format: full"
        ]
    , CAT.nofilter = def &= text "Filter off" & explicit & flag "nofilter"
    } &= text "list known vulnerabilities"

_vulnReportArgs h = mode CAT.VulnReport
    { CAT.config = h &= _configAttrib
    , CAT.nofilter = def &= text "Filter off" & explicit & flag "nofilter"
    } &= text "report (process) vulnerabilities"
