import Sudoku
import System.Environment
import Data.Maybe
import Control.Monad.Par hiding (parMapM)

main :: IO ()
main = do
  [f] <- getArgs
  grids <- fmap lines $ readFile f
  let solutions = runPar $ parMapM (return . solve) grids
  print (length (filter isJust solutions))

parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs
