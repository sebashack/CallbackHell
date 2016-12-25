module ReadWrite where

import FileIO
import Data.Either
import Data.Tuple
import Control.Monad.Cont.Trans
import Control. Monad.Except.Trans
import Data.Traversable
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Data.Function.Uncurried (runFn2, runFn3, runFn4)
import Prelude (Unit, (<<<), ($), (<>), unit, bind, pure)
import Data.String (joinWith)
import Network.HTTP.Client (getEx, URI, HTTP)


readFile :: forall eff .
            FilePath
         -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit)
         -> Eff (fs :: FS | eff) Unit
readFile path k =  
  let curried = runFn3 readFileImpl
  in curried path (k <<< Right) (k <<< Left)     


writeFile :: forall eff .
             FilePath
          -> String
          -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit)
          -> Eff (fs :: FS | eff) Unit 
writeFile path text k = 
  let curried = runFn4 writeFileImpl
  in curried path text (k $ Right unit) (k <<< Left)


-- CURRY SETTIMEOUT AND FIND ContT PATTERN (SOLVED) ***
setTimeOut :: forall eff .
              Milliseconds
           -> (Unit -> (Eff (timeout :: TIMEOUT | eff) Unit)) 
           -> (Eff (timeout :: TIMEOUT | eff) Unit)
setTimeOut mlsecs cb = 
  let curried = runFn2 setTimeOutImpl
  in curried mlsecs (cb unit)
-- ***************************************************


{-
  Now we can spot an important pattern. Each of these functions takes a callback
  which returns a value in some monad and returns a value in the same monad. This
  means that when the first callback returns a result, that monad can be used
  to bind the result to the input of the next asynchrnous function.
-}

-- ContT

type Async eff = ContT Unit (Eff eff)

readFileCont :: forall eff .
                FilePath
             -> Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path 


-- Let's define readFileCont with the (ExceptT e m) monad
readFileContEx :: forall eff .
                  FilePath
               -> ExceptT ErrorCode (Async (fs :: FS | eff)) String 
readFileContEx path = ExceptT $ readFileCont path
-- ******************************************************


writeFileCont :: forall eff .
                 FilePath
              -> String
              -> Async (fs :: FS | eff) (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text



-- Let's write writeFileCont with the (ExceptT e m) monad
writeFileContEx :: forall eff .
                   FilePath
                -> String
                -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
writeFileContEx path text = ExceptT $ writeFileCont path text 


-- WRAP SETTIMEOUT PATTERN IN ContT MONAD (SOLVED) 
setTimeoutCont :: forall eff .
                  Milliseconds
               -> Async (timeout :: TIMEOUT | eff) Unit
setTimeoutCont mlsecs = ContT $ setTimeOut mlsecs 
-- *********************************************



-- Now we can bind the ContT monad transformer in other to implement a copyFile:


copyFileCont :: forall eff .
                FilePath
             -> FilePath
             -> Async (fs :: FS | eff) (Either ErrorCode Unit)
copyFileCont src dest = do
  e <- readFileCont src
  case e of
    Left err -> pure $ Left err
    Right content -> writeFileCont dest content


-- Improved copyFileCont with ExceptT
copyFileContEx :: forall eff .
                  FilePath
               -> FilePath
               -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
copyFileContEx src dest = do 
  content <- readFileContEx src
  writeFileContEx dest content
-- **********************************



concatFilesCont :: forall eff .
                  FilePath
               -> FilePath
               -> FilePath
               -> Async (fs :: FS | eff) (Either ErrorCode Unit)
concatFilesCont src1 src2 dest = do
  e1 <- readFileCont src1
  e2 <- readFileCont src2
  case Tuple e1 e2 of
    Tuple (Left err) _ -> pure $ Left err
    Tuple _  (Left err) -> pure $ Left err
    Tuple (Right txt1) (Right txt2) -> writeFileCont dest (txt1 <> txt2)


-- Improved version of concatFilesCont
concatTwoFilesContEx :: forall eff .
                     FilePath
                  -> FilePath 
                  -> FilePath
                  -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
concatTwoFilesContEx src1 src2 dest = do
  content1 <- readFileContEx src1
  content2 <- readFileContEx src2
  writeFileContEx dest (content1 <> content2)
-- ***********************************

-- Now we can easily concatenate multiple files
concatManyFilesContEx :: forall eff .
                     Array FilePath 
                  -> FilePath
                  -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
concatManyFilesContEx paths dest = do
  files <- traverse readFileContEx paths 
  writeFileContEx dest (joinWith "" files)
-- *******************************************

  
-- Make a request a copy a response into a file
writeRequestEx :: forall eff .
                  URI
               -> FilePath
               -> ExceptT String (Async (fs :: FS, http :: HTTP | eff)) Unit
writeRequestEx req dest = do
  body <- getEx req
  writeFileContEx dest body
-- *******************************************


