import Sudoku
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
      solutions = map solve puzzles

  print (length (filter isJust solutions))

-- compile with
--   ghc -O2 sudoku1.hs -rtsopts
