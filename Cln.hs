import GHC.IO.Handle
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import Data.Maybe
import Data.Char
import qualified Data.Foldable as F
import System.Environment
import System.IO
import System.IO.Error
import Network
import Safe

main = do
  args <- getArgs
  if length args == 2
    then  startCln (args !! 0) (args !! 1)
    else putStrLn $ "Usage: Cln <ip> <port>"

startCln ip port = withSocketsDo $ do
  case (readMay port :: Maybe Int) of
    Nothing    -> putStrLn $ "ERROR Port is not a number"
    Just port' -> do
      let conn       = connectTo ip (PortNumber $ fromIntegral port') >>= return . Just
      let errHndlr _ = return Nothing 
      mhndl <-  conn `catchIOError` errHndlr
      case mhndl of
        Nothing   -> putStrLn "ERROR Can't connect"
        Just hndl -> runCln hndl 

runCln hndl = do
  void $ forkIO $ fix $ \loop -> do
    let recv       = hGetLine hndl >>= putStrLn >> return True
    let errHndlr _ = return False
    again <- recv `catchIOError` errHndlr
    when again loop

  void $ fix $ \loop -> do
    let send       = getLine >>= \line -> hPutStr hndl (line ++ "\n") >> return True
    let errHndlr _ = return False
    again <- send `catchIOError` errHndlr
    when again loop

