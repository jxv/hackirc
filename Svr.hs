import GHC.IO.Handle
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import System.IO
import System.IO.Error
import Network

data ID = ID { getID :: Integer }
  deriving (Show, Eq, Ord)

type Svr a = RWS SvrReader [SvrCmd] SvrState a

data SvrReader = SvrReader -- needed for reader in `Svr' (RWS)
  deriving (Show)

data SvrCmd = 
    SvrOk      ID (Maybe String)
  | SvrError   ID String
  | SvrMessage ID String String String
  | SvrWispher ID String String
  | SvrJoin    ID String String
  | SvrLeave   ID String String
  deriving (Show)

data SvrState = SvrState
  { svrIDs   :: M.Map ID     String
  , svrNames :: M.Map String ID
  , svrRooms :: M.Map String (S.Set ID)
  }
  deriving (Show)

idSvrCmd :: SvrCmd -> ID
idSvrCmd cmd = case cmd of
  SvrOk      idn _     -> idn
  SvrError   idn _     -> idn
  SvrMessage idn _ _ _ -> idn
  SvrWispher idn _ _   -> idn
  SvrJoin    idn _ _   -> idn
  SvrLeave   idn _ _   -> idn

rawSvrCmd :: SvrCmd -> String
rawSvrCmd cmd = (++ "\n") . unwords $ case cmd of
  SvrOk      _ mmsg                   -> ["OK", fromMaybe [] mmsg]
  SvrError   _ msg                    -> ["ERROR", msg]
  SvrMessage _ room   sender  content -> ["MESSAGE", room, sender, content]
  SvrWispher _ sender content         -> ["WISPHER", sender, content]
  SvrJoin    _ room   user            -> ["JOIN", room, user]
  SvrLeave   _ room   user            -> ["LEAVE", room, user]

emptySvrState = SvrState M.empty M.empty M.empty

execSvr s = execRWS s SvrReader

doCmd idn str = do
  let raw = words str
  let (rcmd:rargs) = raw
  let len = length rargs
  unless (null raw) $ action raw len rcmd rargs
  where
    action raw len rcmd rargs
      | null raw = tell [SvrError idn "No Input"]
      | rcmd == "LOGIN"   && len == 1 = login   idn (head rargs)
      | rcmd == "LOGOUT"  && len == 0 = logout  idn 
      | rcmd == "JOIN"    && len == 1 = join'   idn (head rargs)
      | rcmd == "LEAVE"   && len == 1 = leave   idn (head rargs)
      | rcmd == "SAY"     && len >= 1 = say     idn (head rargs) $ unwords $ drop 1 rargs
      | rcmd == "WISPHER" && len >= 1 = wispher idn (head rargs) $ unwords $ drop 1 rargs
      | otherwise = tell [SvrError idn "Invalid Input"]

login idn name = do
  (SvrState idns names rooms) <- get
  case (M.lookup idn idns) of
    Just _ -> tell [SvrError idn "Already logged in"]
    Nothing -> do
      case (M.lookup name names) of
        Just _  -> tell [SvrError idn "Name taken"]
        Nothing -> do
          let idns'  = M.insert idn name idns
          let names' = M.insert name idn names
          put $ SvrState idns' names' rooms
          tell [SvrOk idn $ Just "Welcome"]

logout idn = do
  logged idn $ \name -> do
    (SvrState idns names rooms) <- get
    let roomnames = M.keys $ M.filter (S.member idn) rooms
    forM_ roomnames $ leave idn -- slow
    tell [SvrOk idn $ Just "Logged out"]
    (SvrState idns' names' rooms') <- get
    let idns''  = M.delete idn idns'
    let names'' = M.delete name names'
    put (SvrState idns'' names'' rooms')

join' idn room = do
  logged idn $ \name -> do
    (SvrState idns names rooms) <- get
    let svrstate = SvrState idns names
    tell [SvrOk idn $ Just "Joined room"] 
    case (M.lookup room rooms) of
      Nothing    -> put $ svrstate $ M.insert room (S.singleton idn) rooms
      Just sidns -> put $ svrstate $ M.adjust (S.insert idn) room rooms

leave idn room = do
  logged idn $ \name -> do
    inRoom idn room $ \sidns -> do
      let toidns = S.elems $ S.filter (/= idn) sidns
      tell [SvrLeave i room name | i <- toidns]
      tell [SvrOk idn $ Just "Left room"]
      (SvrState idns names rooms) <- get
      let rooms' = M.adjust (S.delete idn) room rooms
      let rooms'' = M.filter (not . S.null) rooms'
      put (SvrState idns names rooms'')
      
say idn room content = do
  logged idn $ \sender -> do
    inRoom idn room $ \sidns -> do
      let toidns = S.elems $ S.filter (/= idn) sidns
      tell [SvrMessage i room sender content | i <- toidns]
      tell [SvrOk idn $ Just "Sent message"]

wispher idn name content = do
  logged idn $ \sender -> do
    (SvrState _ names _) <- get
    let mrecip = M.lookup name names
    case mrecip of
      Nothing    -> tell [SvrError idn "No such user"]
      Just recip -> tell [SvrWispher recip sender content]

logged idn s = do
  (SvrState idns _ _) <- get
  case (M.lookup idn idns) of
    Nothing   -> tell [SvrError idn "Not logged in"]
    Just name -> s name

inRoom idn room s = do
  (SvrState _ _ rooms) <- get
  case (M.lookup room rooms) of
    Nothing    -> tell [SvrError idn "Not in room"]
    Just sidns -> if S.member idn sidns
      then s sidns
      else tell [SvrError idn "Not in room"]

--

doCommand mvstate idn rawstr = modifyMVar mvstate $ \state ->
  return $ execSvr (doCmd idn rawstr) state

handleCln (idn, hndl) mvmidn mvstate = do
  chcmd <- newChan
  
  modifyMVar mvmidn $ \midn -> return (M.insert idn chcmd midn, ())

  sendThId <- forkIO $ fix $ \loop -> do
    let cmdToChan midn cmd = F.mapM_ ((flip writeChan) cmd) $ M.lookup (idSvrCmd cmd) midn
    let cmdsToChan cmds = readMVar mvmidn >>= \midn -> F.forM_ cmds $ cmdToChan midn
    cmd <- readChan chcmd
    let send = hPutStr hndl (rawSvrCmd cmd) >> return True
    let errHndlr _ = doCommand mvstate idn "LOGOUT" >>= cmdsToChan >> return False
    again <- send `catchIOError` errHndlr
    when again loop

  fix $ \loop -> do
    let cmdToChan midn cmd = F.mapM_ ((flip writeChan) cmd) $ M.lookup (idSvrCmd cmd) midn
    let cmdsToChan cmds = readMVar mvmidn >>= \midn -> F.forM_ cmds $ cmdToChan midn
    let recv = hGetLine hndl >>= doCommand mvstate idn >>= cmdsToChan >> return True
    let errHndlr _ = doCommand mvstate idn "LOGGOUT" >>= cmdsToChan >> killThread sendThId >> return False
    again <- recv `catchIOError` errHndlr
    if again
      then loop
      else modifyMVar mvmidn $ \midn -> return (M.delete idn midn, ())

listener sock rawidn mvmidn mvstate = do
  (hndl,_,_) <- accept sock
  handleCln (ID rawidn, hndl) mvmidn mvstate
  listener sock (rawidn + 1) mvmidn mvstate

main = do
  sock <- listenOn $ PortNumber 1801
  mvmidn  <- newMVar $ M.empty
  mvstate <- newMVar emptySvrState
  listener sock 0 mvmidn mvstate

