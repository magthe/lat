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

module LWN
    ( getAlerts
    ) where

import Control.Monad
import Data.Maybe
import Data.Time.Format
import Network.HTTP
import System.Locale
-- import Text.HTML.Download
import Text.HTML.TagSoup
import Text.Regex

import qualified Types as T

type MyTag = Tag String

--{{{1 useful stuff
getAllTags :: String -> IO [MyTag]
getAllTags = liftM parseTags . openURL
    where
        openURL url = getResponseBody =<< simpleHTTP (getRequest url)

--{{{1 get the number of alerts
getAlertCount :: String -> IO Int
getAlertCount url = liftM (getTheCount .(!! 0)) $ getAllTags url >>= filterM (return . isTheCount)
    where
        regex = mkRegex "([[:digit:]]+) alerts total"

        --isTheCount :: MyTag -> Bool
        isTheCount (TagText s) = isJust $ matchRegex regex s
        isTheCount _ = False

        --getTheCount :: MyTag -> Int
        getTheCount (TagText s) = read $ maybe "0" (!! 0) (matchRegex regex s)
        getTheCount _ = 0

getFullAlertPage url = do
    count <- getAlertCount url
    let url2 = url ++ "?n=" ++ show count
    getAllTags url2

-- I want to extract the table:
--  getFullAlertPage >>= return . (!! 2) .  partitions (~== TagOpen "table" [])
--
--  The 4 parts of the partitioning on <table> (roughly the page is split into
--  [head, table for Google ad stuff, table for navigation, table for content])
extractAlertTable :: [MyTag] -> [MyTag]
extractAlertTable = (!! 3) . partitions (~== TagOpen "table" [])

-- Then I can partition it on (TagOpen "tr" []), drop the first item (it's the
-- header) and then take the number of items equal to the number of alerts.
extractAlertRows :: [MyTag] -> [[MyTag]]
extractAlertRows = tail . partitions (~== TagOpen "tr" [])

-- Each line has the following form:
--
--    [TagOpen "tr" [],
--     TagText "   ",
--     TagOpen "td" [],
--     TagOpen "a" [("href","/Alerts/293022")],  <-- catch the href (prefix with http://lwn.net
--     TagText "CESA-2008:0612",                 <-- catch the ID
--     TagClose "a",
--     TagClose "td",
--     TagText "\n   ",
--     TagOpen "td" [],
--     TagText "kernel",                         <-- catch the package
--     TagClose "td",
--     TagOpen "td" [],
--     TagText "2008-08-06",                     <-- date
--     TagClose "td",
--     TagClose "tr",
--     TagText "\n  "]
alertFromRow :: T.Distro -> [MyTag] -> Maybe T.Alert
alertFromRow distro ts = do
    guard (length ts >= 16)
    return $ T.alert
        [("http://lwn.net" ++ fromAttrib "href" (ts !! 3))]
        (fromTagText $ ts !! 4) -- id
        (fromTagText $ ts !! 9) -- package
        (readTime defaultTimeLocale "%Y-%m-%d" $ fromTagText $ ts !! 12)
        False -- not processed
        distro

--{{{1 turning the full page into alerts
getAlerts :: T.Distro -> IO [T.Alert]
getAlerts distro = do
    let url = fromMaybe ("http://lwn.net/Alerts/" ++ T.distroName distro ++ "/") $ T.distroUrl distro
    page <- getFullAlertPage url
    let rows = extractAlertRows $ extractAlertTable page
    return $ mapMaybe (alertFromRow distro) rows
