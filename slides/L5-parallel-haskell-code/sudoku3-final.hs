import Sudoku
import Control.Exception
import System.Environment

import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
  _ <- evaluate $ length puzzles -- force IO before parallel solving
  let solutions = runEval (parMap solve puzzles)

  print (length (filter isJust solutions))

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b:bs)
