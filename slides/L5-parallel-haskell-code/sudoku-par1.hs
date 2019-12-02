import Sudoku
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  grids <- fmap lines $ readFile f
  let solutions = map solve grids
  print (length (filter isJust solutions))
