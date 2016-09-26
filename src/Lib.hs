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
    [ ("view", view)
    , ("add", add)
    ]

add :: [String] -> IO ()
add = const (putStrLn "adding...")

view :: [String] -> IO ()
view = const (putStrLn "viewing...")
