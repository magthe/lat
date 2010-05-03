{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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

module AlertDB
    ( AlertDB
    , runAlertDB
    -- functions for distros
    , addDistro
    , getDistroById
    , deleteDistro
    , getAllDistro
    -- functions for alerts
    , addAlert
    , getAlert
    , setReportedAlert
    --, deleteAlert
    , getAllAlertId
    , getAllAlert
    , getReportedAlert
    , getUnreportedAlert
    ) where

import Control.Monad.Reader
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import System
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath (joinPath)

import qualified Types as T

type AlertDB a = ReaderT Connection IO a

-- {{{1 general funcs
runAlertDB :: FilePath -> AlertDB a -> IO a
runAlertDB path func = do
    doesDirectoryExist path >>= flip unless (createDirectory path)
    conn <- connectSqlite3 $ joinPath [path, "lat.db"]
    tbls <- getTables conn
    unless ("distro" `elem` tbls) (createTableDistro conn >> commit conn)
    unless ("alert" `elem` tbls) (createTableAlert conn >> commit conn)
    unless ("alerturl" `elem` tbls) (createTableAlertUrl conn >> commit conn)
    res <- runReaderT func conn
    liftIO $ commit conn
    disconnect conn
    return res

failOnException msg = handleSql
    (\ exc -> putStrLn ("Error: " ++ seErrorMsg exc) >> putStrLn msg >> exitFailure)

-- {{{1 create tables
createTableDistro conn = failOnException "Couldn't create table 'distro'" $ run conn
    "CREATE TABLE distro ( id INTEGER PRIMARY KEY , name TEXT NOT NULL UNIQUE , url TEXT UNIQUE);" []

createTableAlert conn = failOnException "Couldn't create table 'alert'" $ run conn
    "CREATE TABLE alert ( id INTEGER PRIMARY KEY , identity TEXT NOT NULL UNIQUE , package TEXT NOT NULL , date DATE NOT NULL , sent BOOLEAN NOT NULL , distro_id INTEGER NOT NULL);" []

createTableAlertUrl conn = failOnException "Couldn't create table 'alerturl'" $ run conn
    "CREATE TABLE alerturl ( alertidentity TEXT, url TEXT NOT NULL );" []


-- {{{1 distro
-- | Add a distribution to the database.
addDistro :: T.Distro -> AlertDB ()
addDistro (T.Distro _ name Nothing) = do
    conn <- ask
    liftIO $ failOnException "Couldn't add distribution, are you sure it isn't added already?" $ run conn "INSERT INTO distro (name) VALUES (?)" [ toSql name ]
    return ()

addDistro (T.Distro _ name url) = do
    conn <- ask
    liftIO $ failOnException "Couldn't add distribution, are you sure it isn't added already?" $ run conn "INSERT INTO distro (name, url) VALUES (?, ?)"
        [ toSql name
        , toSql $ fromJust url
        ]
    return ()

-- | Get a distro from the database based on ID
_list2Distro :: [SqlValue] -> T.Distro
_list2Distro [sid, sname, surl] = T.Distro
    (fromSql sid)
    (fromSql sname)
    (fromSql surl)
_list2Distro _ = error "_list2Distro"

getDistroById :: Int -> AlertDB (Maybe T.Distro)
getDistroById distroID = do
    conn <- ask
    sd <- liftIO $ failOnException "" $ quickQuery' conn "SELECT id, name, url FROM distro WHERE id = ?" [toSql distroID]
    return $ if null sd
        then Nothing
        else Just $ _list2Distro $ head sd

-- | Delete a distribution from the database.
deleteDistro :: T.Distro -> AlertDB ()
deleteDistro d = do
    conn <- ask
    liftIO $ failOnException "" $ run conn "DELETE FROM distro WHERE name=?" [toSql $ T.distroName d]
    return ()

-- | List all distributions in the database.
getAllDistro :: AlertDB [T.Distro]
getAllDistro = do
    conn <- ask
    sd <- liftIO $ failOnException "" $ quickQuery' conn "SELECT * FROM distro" []
    return $ map _list2Distro sd

addAlertUrl :: String -> String -> AlertDB ()
addAlertUrl aident url = do
    conn <- ask
    oldUrls <- getAlertUrls aident
    if url `elem` oldUrls
        then return ()
        else do
            liftIO $ failOnException ("Couldn't add alert URL " ++ url ++ " to alert " ++ aident) $ run conn "INSERT INTO alerturl (alertidentity, url) VALUES (?, ?)"
                [ toSql aident
                , toSql url
                ]
            return ()

getAlertUrls :: String -> AlertDB [String]
getAlertUrls aident = do
    conn <- ask
    urls <- liftIO $ failOnException ("Couldn't find URLs for " ++ aident) $ quickQuery' conn "SELECT url FROM alerturl WHERE alertidentity=?" [toSql aident]
    return $ map fromSql $ concat urls

-- -- {{{1 alert
addAlert :: T.Alert -> AlertDB ()
addAlert alert@(T.Alert _ urls identity package date proc distro) = let
        doAddAlert = do
            conn <- ask
            liftIO $ failOnException ("Couldn't add alert: " ++ show alert) $ run conn "INSERT INTO alert (identity, package, date, sent, distro_id) VALUES (?, ?, ?, ?, ?)"
                [ toSql identity
                , toSql package
                , toSql date
                , toSql proc
                , toSql $ fromJust $ T.distroId distro
                ]
            mapM_ (addAlertUrl identity) urls
            -- liftIO $ commit conn  -- silly, but seems to help when adding many vulns
            return ()
        doAddUrlToAlert url = do
            addAlertUrl identity url
            unsetReportedAlertId identity
    in do
        b <- getAlert identity
        maybe doAddAlert (const $ mapM_ doAddUrlToAlert urls) b

-- | Get an alert with a specific identity.
getAlert :: String -- ^ Identity of alert
    -> AlertDB (Maybe T.Alert)
getAlert identity = do
    conn <- ask
    sd <- liftIO $ failOnException "getAlert" $ quickQuery' conn "SELECT * FROM alert WHERE identity=?" [toSql identity]
    if null sd
        then return Nothing
        else liftM Just (_list2Alert $ head sd)

setReportedAlert :: Int -> AlertDB ()
setReportedAlert alertId = do
    conn <- ask
    liftIO $ failOnException "" $ run conn "UPDATE alert SET sent=? WHERE id=?"
        [ toSql True, toSql alertId]
    return ()

unsetReportedAlertId aident = do
    conn <- ask
    liftIO $ failOnException "" $ run conn "UPDATE alert SET sent=? WHERE identity=?"
        [ toSql False, toSql aident ]
    return ()

getAllAlertId :: AlertDB [String]
getAllAlertId = do
    conn <- ask
    sd <- liftIO $ failOnException "" $ quickQuery' conn "SELECT identity FROM alert" []
    return $ map fromSql $ concat sd

_list2Alert :: [SqlValue] -> AlertDB T.Alert
_list2Alert [aid, aident, pkg, date, proc, distroid] = do
    d <- getDistroById (fromSql distroid)
    us <- getAlertUrls (fromSql aident)
    return $ T.Alert
        (fromSql aid)
        us
        (fromSql aident)
        (fromSql pkg)
        (fromSql date)
        (fromSql proc)
        (fromJust d)
_list2Alert _ = error "_list2Alert"

_getAlert query = do
        conn <- ask
        sd <- liftIO $ failOnException "" $ quickQuery' conn query []
        mapM _list2Alert sd

getAllAlert :: AlertDB [T.Alert]
getAllAlert = _getAlert "SELECT * FROM alert"

getReportedAlert :: AlertDB [T.Alert]
getReportedAlert = _getAlert "SELECT * FROM alert WHERE sent='True'"

getUnreportedAlert :: AlertDB [T.Alert]
getUnreportedAlert = _getAlert "SELECT * FROM alert WHERE sent='False'"
