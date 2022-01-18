module Exercise3 where
import Control.Monad (when)

type Commands = [(Command, Action)]
type Command = String
type Action = [Argument] -> IO ()
type Argument = String

commands = [("echo", \args -> putStrLn (unwords args))]

doCommands :: Commands -> IO ()
doCommands commands = do
    putStr "> "
    line <- getLine
    let ws = words line
    if null ws
    then putStrLn "goodbye"
    else let    cmd  = head ws
                args = tail ws
        in case lookup cmd commands of
                Just io -> do {io args; doCommands commands}
                nothing -> do {putStrLn (cmd ++ " is not a known command"); doCommands commands}