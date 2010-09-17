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
    -- &= help "Linux Alert Tracker - track alerts on LWN.net" &= program "lat" &= summary ver
    cmdArgs (_allCmds home) >>= _doCommand

_doCommand a@(CAT.DistroAdd {}) = D.distroAdd a
_doCommand a@(CAT.DistroList {}) = D.distroList a
_doCommand a@(CAT.VulnUpdate {}) = V.vulnUpdate a
_doCommand a@(CAT.VulnList {}) = V.vulnList a
_doCommand a@(CAT.VulnReport {}) = V.vulnReport a

-- {{{1 Command line arg definitions
_allCmds h = modes cmds &= help "Linux Alert Tracker - track alerts on LWN.net" &= program "lat" &= summary ver
    where
        cmds = map (\ f -> f h) [_distroAddArgs, _distroListArgs, _vulnUpdateArgs, _vulnListArgs, _vulnReportArgs]
        ver = "lat v" ++ (showVersion version) ++ " Copyright 2010 Magnus Therning <magnus@therning.org>"

_configAttrib = help "Set dir for configuration" &= opt "~/.lat" &= typDir

_distroAddArgs h = CAT.DistroAdd
    { CAT.config = h &= _configAttrib
    , CAT.name = def &= argPos 0 &= typ "NAME"
    , CAT.url = def &= argPos 1 &= typ "URL"
    } &= help "add a distribution"

_distroListArgs h = CAT.DistroList
    { CAT.config = h &= _configAttrib
    } &= help "list distributions"

_vulnUpdateArgs h = CAT.VulnUpdate
    { CAT.config = h &= _configAttrib
    , CAT.dry = def &= help "Dry run"
    } &= help "update the vulnerability database"

_vulnListArgs h = CAT.VulnList
    { CAT.config = h &= _configAttrib
    , CAT.typ = enum
        [ CAT.Unreported &= help "List unreported issues (default)"
        , CAT.Reported &= help "List reported issues"
        , CAT.All &= help "List all issues"
        ]
    , CAT.size = enum
        [ CAT.Small &= help "Output format: small"
        , CAT.Normal &= help "Output format: normal" &= name "n"
        , CAT.Full &= help "Output format: full"
        ]
    , CAT.nofilter = def &= help "Filter off" &= explicit &= name "nofilter"
    } &= help "list known vulnerabilities"

_vulnReportArgs h = CAT.VulnReport
    { CAT.config = h &= _configAttrib
    , CAT.nofilter = def &= help "Filter off" &= explicit &= name "nofilter"
    } &= help "report (process) vulnerabilities"
