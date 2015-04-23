import System.Environment
import Data.Foldable
import Control.Exception
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

-- Usage: youguts [args]
-- Scope: Ghost in the shell typing sym

handleIn :: String -> IO ()
handleIn [] = do return ()
handleIn a = interactChar $ cycle a

interactChar :: String -> IO ()
interactChar (x:xs) = do
    k <- getCh
    case k of
        KeyChar '\x03' -> return ()
        _              -> do
                            wAddStr stdScr [x]
                            interactChar xs

main :: IO ()
main = do
    start
    s <- getArgs
    filesContents <- mapM readFile s
    handleIn $ unlines filesContents 
    `finally` end
