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

module Types where

import Data.Maybe
import Data.Time.Calendar
--import Data.Time.Clock
import Text.PrettyPrint.ANSI.Leijen

data Distro = Distro
    { distroId :: Maybe Int     -- ^ DB ID, automatically populated by the DB
    , distroName :: String
    , distroUrl :: Maybe String -- ^ URL to use for lookup, if it's 'Nothing' then @distroName@ will be used to construct the URL
    } deriving (Show, Eq)

data Alert = Alert
    { alertId :: Maybe Int      -- ^ DB ID, automatically populated by the DB
    , alertUrl :: [String]
    , alertIdentity :: String   -- ^ identity as reported by LWN
    , alertPackage :: String
    , alertDate :: Day
    , alertProcessed :: Bool    -- ^ record of whether the alert has been processed by lat before
    , alertDistro :: Distro
    } deriving (Show, Eq)

distro = Distro Nothing
alert = Alert Nothing

getAlertDistroName :: Alert -> String
getAlertDistroName = distroName . alertDistro

-- | Class for pretty output of different levels of verbosity.
class PrettyLevels a where
    prettyTerse :: a -> Doc
    prettyTerse = prettyNormal

    prettyNormal :: a -> Doc

    prettyVerbose :: a -> Doc
    prettyVerbose = prettyNormal

instance PrettyLevels Distro where
    prettyNormal d = let
            name = pretty $ distroName d
            url = parens $ text $ fromMaybe "-" (distroUrl d)
        in name <> space <> url

instance PrettyLevels Alert where
    prettyTerse a = let
            urls = foldl (<+>) empty . map text $ alertUrl a
            identity = text $ alertIdentity a
            package = text $ alertPackage a
        in identity <+> colon <+> package <+> colon <+> urls

    prettyNormal a = let
            urls = foldl (<+>) empty . map text $ alertUrl a
            identity = text $ alertIdentity a
            package = text $ alertPackage a
            date = pretty $ alertDate a
            processed = pretty $ alertProcessed a
            distro = prettyNormal $ alertDistro a
        in package <+> colon <+> identity
            <$> indent 2 (date <+> distro <+> processed <+> urls)

instance Pretty Day where
    pretty = text . show
