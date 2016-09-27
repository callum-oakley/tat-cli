module Lib
    ( tat
    ) where

import System.Environment

tat :: IO ()
tat = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

dispatch :: [(String, [String] -> IO ())]
dispatch =
    [ ("add", add)
    , ("list", list)
    ]

add :: [String] -> IO ()
add [task] = appendFile "/Users/callum/Dropbox/.tat/tasks" (task ++ "\n")

list :: [String] -> IO ()
list _ = do
    contents <- readFile "/Users/callum/Dropbox/.tat/tasks"
    let tasks = lines contents
        addLineNumber n line =
            (replicate (2 - length (show n)) ' ' ++ show n) ++ "  " ++ line
        numberedTasks = zipWith addLineNumber [0..] tasks
    putStr $ unlines numberedTasks
