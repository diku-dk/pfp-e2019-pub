import Sudoku
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq (force)
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file

      (as,bs) = splitAt (length puzzles `div` 2) puzzles

      solutions = runEval $ do
                    as' <- rpar $ force $ map solve as
                    bs' <- rpar $ force $ map solve bs
                    return (as' ++ bs')

  print (length (filter isJust solutions))

-- compile with
--   ghc -O2 sudoku2.hs -rtsopts -threaded

-- compile with support for event logging
--   ghc -O2 sudoku2.hs -rtsopts -threaded -eventlog
