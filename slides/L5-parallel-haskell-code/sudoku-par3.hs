import Sudoku
import System.Environment
import Data.Maybe
import Control.Monad.Par

main :: IO ()
main = do
  [f] <- getArgs
  grids <- fmap lines $ readFile f
  let solutions = runPar $ parMap solve grids
  print (length (filter isJust solutions))
