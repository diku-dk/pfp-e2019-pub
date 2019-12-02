import Sudoku
import System.Environment
import Data.Maybe
import Control.Monad.Par

main :: IO ()
main = do
  [f] <- getArgs
  grids <- fmap lines $ readFile f

  let (as,bs) = splitAt (length grids `div` 2) grids
      solutions = runPar $ do
        i1 <- new
        i2 <- new
        fork $ put i1 (map solve as)
        fork $ put i2 (map solve bs)
        as' <- get i1
        bs' <- get i2

        return (as' ++ bs')

  print $ length $ filter isJust solutions


-- The Par Monad
-- ==============
-- data Par
-- instance Monad Par

-- runPar :: Par a -> a
-- fork :: Par () -> Par ()

-- data IVar
-- new :: Par (IVar a)
-- get :: IVar a -> Par a
-- put :: NFData a => IVar a -> a -> Par ()
