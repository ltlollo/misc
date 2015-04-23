import Control.Monad
import System.Environment
import Data.Foldable
import Control.Exception
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

-- scrollok()?

usage = "Usage: yougits [args]\nScope: Ghost in the shell typing symulator"

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
    args <- getArgs
    when (null args) $ fail usage
    filesContents <- mapM readFile args
    handleIn $ unlines filesContents 
    `finally` end
