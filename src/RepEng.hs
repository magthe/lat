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

module RepEng where

import Control.Monad.Error
import Control.Exception as CE
import Data.ConfigFile
import Data.Either.Utils
import Data.Maybe
import Network.XmlRpc.Client
import Network.XmlRpc.Internals
import System.Directory
import System.FilePath

import qualified AlertDB as ADB
import qualified Types as T

-- assoc of backend name and function (taking path to database and the alert)
type Engine = (String, FilePath -> T.Alert -> IO ())

allEngines :: [Engine]
allEngines = [ ("jira", _jiraReport)]

_getConfigFile :: IO ConfigParser
_getConfigFile = let
        gCF h = runErrorT $ join $ liftIO $ readfile emptyCP (combine h "lat.conf")
    in do
        home <- getAppUserDataDirectory "lat"
        conf <- CE.catch (gCF home) (\ e -> return $ Left (OtherProblem "IO Error", show (e :: CE.SomeException)))
        -- TODO: add some better error handling here, or at least reporting to the user
        return $ either (const emptyCP) id conf

data JiraConfig = JiraConfig
    { jiraURL :: String
    , jiraUser :: String
    , jiraPwd :: String
    , jiraProject :: String
    , jiraType :: String
    , jiraPriority :: String
    , jiraComponent :: String
    } deriving (Show)

_getWithDefault cfg section option def = either (const def) id (get cfg section option)

_getJiraConfig cfg = JiraConfig url user pwd project typ priority component
    where
        url = _getWithDefault cfg "jira" "url" "NoUrl" -- http://scale-dev.uk.xensource.com/rpc/xmlrpc
        user = _getWithDefault cfg "jira" "user" "NoUser" -- magnusth
        pwd = _getWithDefault cfg "jira" "pwd" "NoPassword"
        project = _getWithDefault cfg "jira" "project" "NoProject" -- Carbon
        typ = _getWithDefault cfg "jira" "type" "NoType" -- Task
        priority = _getWithDefault cfg "jira" "priority" "NoPriority" -- Major
        component = _getWithDefault cfg "jira" "component" "NoComponent" -- Security

_jiraReport :: FilePath -> T.Alert -> IO ()
_jiraReport dbpath a = do
    cfg <- liftM _getJiraConfig _getConfigFile
    bracket
        (_jiraLogin (jiraURL cfg) (jiraUser cfg) (jiraPwd cfg))
        (_jiraLogout (jiraURL cfg))
        (\ s -> do
            --proj <- _getProjectKey (jiraURL cfg) s (jiraProject cfg)
            --typ <- _getTypeId (jiraURL cfg) s (jiraType cfg)
            --pri <- _getPriorityId (jiraURL cfg) s (jiraPriority cfg)
            --comp <- _getComponentId (jiraURL cfg) s proj (jiraComponent cfg)
            key <- _createIssue cfg a s
            putStrLn $ "Recorded as " ++ key
            ADB.runAlertDB dbpath (ADB.setReportedAlert (fromJust $ T.alertId a)))

_jiraLogin :: String    -- ^ url
    -> String           -- ^ username
    -> String           -- ^ password
    -> IO String        -- ^ session token
_jiraLogin url = remote url "jira1.login"

_jiraLogout :: String   -- ^ url
    -> String           -- ^ session token
    -> IO Bool
_jiraLogout url = remote url "jira1.logout"

_jiraGetProjects :: String -> String -> IO [[(String, String)]]
_jiraGetProjects url = remote url "jira1.getProjects"

_jiraGetTypes :: String -> String -> IO [[(String, String)]]
_jiraGetTypes url = remote url "jira1.getIssueTypes"

_jiraGetPriorities :: String -> String -> IO [[(String, String)]]
_jiraGetPriorities url = remote url "jira1.getPriorities"

_jiraGetComponents :: String    -- ^ url
    -> String                   -- ^ session token
    -> String                   -- ^ project name
    -> IO [[(String, String)]]
_jiraGetComponents url = remote url "jira1.getComponents"

_jiraCreateIssue :: String  -- ^ url
    -> String               -- ^ session token
    -> Value                -- ^ issue description
    -> IO String
_jiraCreateIssue url session desc = do
    res <- remote url "jira1.createIssue" session desc
    liftM fromRight $ runErrorT (fromValue $ fromJust $ lookup "key" res)

_jiraUpdateIssue :: String  -- ^ url
    -> String               -- ^ session token
    -> String               -- ^ issue key (e.g. "CA-12345")
    -> Value                -- ^ issue description
    -> IO String
_jiraUpdateIssue url session issue desc = do
    res <- remote url "jira1.updateIssue" session issue desc
    liftM fromRight $ runErrorT (fromValue $ fromJust $ lookup "key" res)

_getJiraValue jiraFunc matchKey matchValue key = let
        filterF p = lookup matchKey p == Just matchValue
    in do
        projects <-  jiraFunc
        return . fromJust . lookup key $ head (filter filterF projects)

_getProjectKey url session project = _getJiraValue (_jiraGetProjects url session) "name" project "key"
_getTypeId url session typ = _getJiraValue (_jiraGetTypes url session) "name" typ "id"
_getPriorityId url session priority = _getJiraValue (_jiraGetPriorities url session) "name" priority "id"
_getComponentId url session project component = _getJiraValue (_jiraGetComponents url session project) "name" component "id"

-- for some strange reason I can't manage to set the components of an issue as
-- its created, hence the two calls
_createIssue cfg alert session = let
        summary = "(Upstream security issue) " ++ show (T.prettyTerse alert)
        description = show $ T.prettyVerbose alert
        url = jiraURL cfg
    in do
        project <- _getProjectKey url session (jiraProject cfg)
        typ <- _getTypeId url session (jiraType cfg)
        priority <- _getPriorityId url session (jiraPriority cfg)
        component <- _getComponentId url session project (jiraComponent cfg)
        let issueDesc = toValue [("project", project), ("type", typ), ("priority", priority), ("summary", summary), ("assignee", jiraUser cfg), ("description", description)]
        newIssue <- _jiraCreateIssue url session issueDesc
        let issueDescUpdate = toValue [("components", [component])]
        _jiraUpdateIssue url session newIssue issueDescUpdate

-- {- ********************************************* -}
-- import Data.Time.Calendar
-- _sillyDistro = T.distro "name" $ Just "url"
-- _sillyAlert = T.alert ["url"] "identity" "package" (ModifiedJulianDay 42) False _sillyDistro
-- _sillyDesc = [("project", "CA"), ("type", "3"), ("priority", "3"), ("summary", "MT: another summary"), ("assignee", "magnusth"), ("description", "Description of issue created with lat")]
-- _sillyUpdate = [("components", ["10095"])]
-- __url = "http://scale-dev.uk.xensource.com/rpc/xmlrpc"
-- 
-- {-
--     sess <- _jiraLogin __url "magnusth" "
--     ik <- _jiraCreateIssue __url sess (toValue _sillyDesc)
--     _jiraUpdateIssue __url sess ik (toValue _sillyUpdate)
-- -}
