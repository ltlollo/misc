import System.Environment
import Data.Foldable
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

handleIn :: String -> IO ()
handleIn [] = do return ()
handleIn a = interactChar (cycle a)

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
    fileContent <- mapM readFile s
    let f =  unlines fileContent 
    handleIn f
    end
