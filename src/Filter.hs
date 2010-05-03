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

import qualified Types as T

import Control.Monad.Error
import Data.ConfigFile
import Data.Either.Utils
import Data.Maybe
import System.Directory
import System.FilePath
import Text.Regex
import qualified Control.Exception as CE

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
        if (not $ has_section cfg "packages")
            then return []
            else do
                let is = forceEither $ items cfg "packages"
                sequence $ map readRegexFile is

-- _getConfigFile :: IO ConfigParser
_getConfigFile = let
        gCF h = runErrorT $ join $ liftIO $ readfile emptyCP (h </> "lat.conf")
    in do
        home <- getAppUserDataDirectory "lat"
        conf <- CE.catch (gCF home) (\ e -> return $ Left (OtherProblem "IO Error", show (e :: CE.SomeException)))
        -- TODO: add some better error handling here, or at least reporting to the user
        return $ either (const emptyCP) id conf

-- _doFilter :: [(String, [Regex])] -> [T.Alert] -> [T.Alert]
_doFilter fs = filter (_isGoodalert fs)

-- _isGoodalert :: [(String, [Regex])] -> T.Alert -> Bool
_isGoodalert fs a = let
        dn = T.getAlertDistroName a
        mrs = lookup dn fs
        an = T.alertPackage a
        isGoodName name = not . null . catMaybes . map (\ r -> matchRegex r name)
    in maybe True (isGoodName an) mrs
