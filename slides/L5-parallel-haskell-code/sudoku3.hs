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
  _ <- evaluate $ length puzzles
  let solutions = runEval $ parMap solve puzzles

  print (length (filter isJust solutions))

-- We would like a parallel map, what should the type be?

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (x : xs) = do
  x' <- rpar $ f x
  xs' <- parMap f xs
  return $ x' : xs'
