import Control.Monad
import System.Environment
import System.Directory
import Data.Set(fromList, toList)
import System.IO.Strict as SIO

usage = "USAGE: storage [files..]"

readFiles :: [FilePath] -> IO [String]
readFiles = mapM SIO.readFile

uniqueLines :: [String] -> String
uniqueLines = unlines . toList . fromList . concat . (map lines)

main = do
    args <- getArgs
    when (null args) $ fail usage
    files <- readFiles $ args
    let origFile = head args
    writeFile origFile $ uniqueLines files
