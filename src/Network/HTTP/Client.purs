module Network.HTTP.Client where

import Data.Function.Uncurried (Fn3, runFn3)
import Control.Monad.Eff
import Control. Monad.Except.Trans
import Data.Either
import Prelude (Unit)
import Control.Monad.Cont.Trans
import Prelude (Unit, (<<<), ($), (<>), unit, bind, pure)

type URI = String
type Async eff = ContT Unit (Eff eff)

foreign import data HTTP :: !
foreign import getImpl :: forall eff .
                          Fn3 URI
                              (String -> Eff (http :: HTTP | eff) Unit)
                              (String -> Eff (http :: HTTP | eff) Unit)
                              (Eff (http :: HTTP | eff) Unit)


                 
get :: forall eff . 
       URI -> Async (http :: HTTP | eff) (Either String String)
get req = ContT \k -> let curried = runFn3 getImpl 
                      in curried req (k <<< Right) (k <<< Left)


-- Improved Version with ExceptT
getEx :: forall eff .
         URI -> ExceptT String (Async (http :: HTTP | eff)) String 
getEx req = ExceptT $ get req


