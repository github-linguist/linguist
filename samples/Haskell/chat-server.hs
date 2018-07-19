{-# LANGUAGE OverloadedStrings #-}
import Network
import System.IO
import Control.Concurrent
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.Reader
import Control.Monad.Error
import Control.Exception
import Data.Monoid
import Control.Applicative

type ServerApp = ReaderT ThreadData IO
data Speaker = Server | Client Text
data ThreadData = ThreadData { threadHandle :: Handle
                             , userTableMV :: MVar (Map Text Handle)}

echoLocal = liftIO . T.putStrLn
echoRemote = echoMessage . (">> "<>)
echoMessage msg = viewHandle >>= \h -> liftIO $ T.hPutStrLn h msg
getRemoteLine = viewHandle >>= liftIO . T.hGetLine
putMVarT = (liftIO.) . putMVar
takeMVarT = liftIO . takeMVar
readMVarT = liftIO . readMVar
modifyUserTable fn = viewUsers >>= \mv ->
                     liftIO $ modifyMVar_ mv (return . fn)
viewHandle = threadHandle <$> ask
viewUsers = userTableMV <$> ask

userChat :: ServerApp ()
userChat = do
    name <- addUser
    echoLocal name
    h <- viewHandle
    (flip catchError) (\_ -> removeUser name) $
      do echoLocal $ "Accepted " <> name
         forever $ getRemoteLine >>= broadcast (Client name)

removeUser :: Text -> ServerApp ()
removeUser name = do
    echoLocal $ "Exception with " <> name <> ", removing from userTable"
    broadcast Server $ name <> " has left the server"
    modifyUserTable (M.delete name)

addUser :: ServerApp Text
addUser = do
    h <- viewHandle
    usersMV <- viewUsers
    echoRemote "Enter username"
    name <- T.filter (/='\r') <$> getRemoteLine
    userTable <- takeMVarT usersMV
    if name `M.member` userTable
      then do echoRemote "Username already exists!"
              putMVarT usersMV userTable
              addUser
      else do putMVarT usersMV (M.insert name h userTable)
              broadcast Server $ name <> " has joined the server"
              echoRemote "Welcome to the server!\n>> Other users:"
              readMVarT usersMV >>=
                  mapM_ (echoRemote . ("*" <>) . fst)
                . filter ((/=name). fst) . M.toList
              return name

broadcast :: Speaker -> Text -> ServerApp ()
broadcast user msg =
    viewUsers >>= readMVarT >>= mapM_ (f . snd) . fn . M.toList
  where f h = liftIO $ T.hPutStrLn h $ nm <> msg
        (fn, nm) = case user of
                    Server -> (id, ">> ")
                    Client t -> (filter ((/=t) . fst), t <> "> ")

clientLoop socket users = do
    (h, _, _) <- accept socket
    hSetBuffering h LineBuffering
    forkIO $ runReaderT userChat (ThreadData h users)
    clientLoop socket users

main = do
    server <- listenOn $ PortNumber 5002
    T.putStrLn "Server started"
    newMVar (M.empty) >>= clientLoop server
