module Main where

import Prelude
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Parallel.Class (parallel, sequential)
import ReadWrite (readFileContEx)
import FileIO (FS)
import Control.Monad.Except (runExceptT)



-- This is an example of a parallel computation using the Parallel
-- type class and the power of Applicatives.
main :: forall e. Eff (fs :: FS, console :: CONSOLE | e) Unit
main = (flip runContT) logShow (runExceptT compt)
  where
    compt = let t1 = "/tmp/file1.txt"
                t2 = "/tmp/file2.txt"
            in sequential $
                 append <$> parallel (readFileContEx t1) <*> parallel (readFileContEx t2)  




    
