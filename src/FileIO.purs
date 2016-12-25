module FileIO where

import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Control.Monad.Eff
import Prelude (Unit)


type ErrorCode = String
type FilePath = String
type Milliseconds = Int 

foreign import data FS :: !
foreign import readFileImpl :: forall eff .
                               Fn3 FilePath
                                   (String -> Eff (fs :: FS | eff ) Unit)
                                   (ErrorCode -> Eff (fs :: FS | eff) Unit)
                                   (Eff (fs :: FS | eff) Unit)              


foreign import writeFileImpl :: forall eff . 
                                Fn4 FilePath
                                    String
                                    (Eff (fs :: FS | eff) Unit)
                                    (ErrorCode -> Eff (fs :: FS | eff) Unit)
                                    (Eff (fs :: FS | eff) Unit)


-- Let's use the FFI to represent the setTimeout function.


-- FOREIGN IMPORT FOR SETTIMEOUT (SOLVED) *********************
foreign import data TIMEOUT :: ! 

foreign import setTimeOutImpl :: forall eff .
                                 Fn2 Milliseconds
                                     (Eff (timeout :: TIMEOUT | eff) Unit) 
                                     (Eff (timeout :: TIMEOUT | eff) Unit)

-- ************************************************************

