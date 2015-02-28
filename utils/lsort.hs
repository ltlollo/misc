import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import Data.List
import Data.Ord

main :: IO ()
main = Txt.interact $ Txt.unlines . reverse . lenSort . Txt.lines
    where lenSort = sortBy $ comparing Txt.length
