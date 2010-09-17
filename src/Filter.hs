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

module Filter where

import Data.Maybe
import System.Directory
import System.FilePath
import Text.Regex
import qualified Data.Ini as Ini
import qualified Data.Ini.Reader as IniR

import qualified Types as T

filterAlerts :: [T.Alert] -> IO [T.Alert]
filterAlerts as = do
    filters <- _getFilters
    return $ _doFilter filters as

-- Load the config file, extract all entries in the sections [filters], each
-- value is a filename, each file is loaded, and the [(distroName, [lines])] is
-- compiled
-- _getFilters :: IO [(String, [Regex])]
_getFilters = let
        readRegexFile (d, fn) = do
            home <- getAppUserDataDirectory "lat"
            rxs <- lines `fmap` readFile (home </> fn)
            return $ (d, map mkRegex rxs)

    in do
        cfg <- _getConfigFile
        if (not $ Ini.hasSection "packages" cfg)
            then return []
            else do
                -- let is = forceEither $ items cfg "packages"
                let is = Ini.allItems "packages" cfg
                sequence $ map readRegexFile is

-- _getConfigFile :: IO Data.Ini.Types.Config
_getConfigFile = do
    home <- getAppUserDataDirectory "lat"
    confStr <- readFile $ home </> "lat.conf"
    let conf = IniR.parse confStr
    case conf of
        Left _ -> return Ini.emptyConfig
        Right c -> return c

-- _doFilter :: [(String, [Regex])] -> [T.Alert] -> [T.Alert]
_doFilter fs = filter (_isGoodalert fs)

-- _isGoodalert :: [(String, [Regex])] -> T.Alert -> Bool
_isGoodalert fs a = let
        dn = T.getAlertDistroName a
        mrs = lookup dn fs
        an = T.alertPackage a
        isGoodName name = not . null . catMaybes . map (\ r -> matchRegex r name)
    in maybe True (isGoodName an) mrs
