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

module Report where

import Data.List
import Data.Maybe
import System.Exit
import System.IO

import qualified AlertDB as ADB
import qualified RepEng as RE
import qualified Types as T

report :: FilePath -> [T.Alert] -> IO ()
report dbpath alerts = let
        engs = RE.allEngines
    in do
    _printEngines engs
    mapM_ (_report dbpath engs) alerts

_printEngines engines = let
        numeng = zip ([1..] :: [Int]) (map fst engines)
        _ne2s ne = (show . fst $ ne) ++ " : " ++ snd ne
    in do
        putStrLn "Report engines:"
        mapM_ (putStrLn . _ne2s) numeng

_report dbpath engines alert = let
        _engChoices = map show (take (length engines) ([1..] :: [Int]))
        _engStr = foldl (\ a b -> a ++ b ++ "/") "" _engChoices

        _withNoStdinBuffering foo = do
            old <- hGetBuffering stdin
            hSetBuffering stdin NoBuffering
            result <- foo
            hSetBuffering stdin old
            return result

        _allChoices = _engChoices ++ ["s", "m", "q"]
        _allChoicesString = foldl (\ a b -> a ++ b ++ "/") "" _allChoices
        _getValidChar = let
                _printInteractiveHelp = do
                    _printEngines engines
                    putStrLn "s : skip"
                    putStrLn "m : mark as done"
                    putStrLn "q : quit reporting"
                    putStrLn "h : print this help"
                _isValid c
                    | c == 'h' = _printInteractiveHelp >> _getValidChar
                    | [c] `elem` _allChoices = return c
                    | otherwise = putStrLn ("Invalid choice: " ++ [c]) >> _getValidChar
            in do
                putStr (_allChoicesString ++ "h ") >> hFlush stdout
                _withNoStdinBuffering (getChar >>= (\ c -> putStrLn "" >> return c)) >>= _isValid

        _processWithEngine = snd
        _processAlert c
            | c == 'q' = exitWith ExitSuccess
            | c == 'm' = ADB.runAlertDB dbpath (ADB.setReportedAlert (fromJust (T.alertId alert)))
            | c == 's' = return ()
            | [c] `elem` _engChoices = _processWithEngine (engines !! fromJust (elemIndex [c] _engChoices)) dbpath alert
            | otherwise = putStrLn $ "Unknown choice: " ++ [c] -- not possible!

    in do
        print $ T.prettyNormal alert
        _getValidChar >>= _processAlert >> putStrLn ""
