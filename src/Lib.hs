module Lib
    ( tat
    ) where

import Data.List
import System.Environment
import System.Directory
import System.IO

path :: String
path = "/Users/callum/Dropbox/.tat/tasks"

tempPath :: String
tempPath = "/Users/callum/Dropbox/.tat/.tasks"

tat :: IO ()
tat = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

dispatch :: [(String, [String] -> IO ())]
dispatch =
    [ ("add", add)
    , ("del", del)
    , ("list", list)
    ]

add :: [String] -> IO ()
add [task] = appendFile path (task ++ "\n")

del :: [String] -> IO ()
del [lineString] = do
    handle <- openFile path ReadMode
    tempHandle <- openFile tempPath WriteMode
    contents <- hGetContents handle
    let line = read lineString
        tasks = lines contents
        tasks' = delete (tasks !! line) tasks
    hPutStr tempHandle $ unlines tasks'
    hClose handle
    hClose tempHandle
    removeFile path
    renameFile tempPath path

list :: [String] -> IO ()
list _ = do
    contents <- readFile path
    let tasks = lines contents
        addLineNumber n line =
            (replicate (2 - length (show n)) ' ' ++ show n) ++ "  " ++ line
        numberedTasks = zipWith addLineNumber [0..] tasks
    putStr $ unlines numberedTasks
