{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Command where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Data.Text (Text, intercalate, pack)
import Data.Text.IO qualified as T
import Data.Time (getCurrentTime)
import Reflex
    ( MonadHold
    , PerformEvent (..)
    , Reflex (Dynamic, Event)
    , TriggerEvent (newTriggerEvent)
    , ffor
    , foldDyn
    )
import System.Exit (ExitCode)
import System.IO
    ( BufferMode (..)
    , Handle
    , hIsEOF
    , hSetBuffering
    , hSetEncoding
    , utf8
    )
import System.Process
    ( CmdSpec (..)
    , CreateProcess (..)
    , StdStream (..)
    , createProcess
    , readCreateProcessWithExitCode
    , shell
    , withCreateProcess
    )

runCommand :: String -> IO (ExitCode, String, String)
runCommand = ($ mempty) . readCreateProcessWithExitCode . shell

pipeOutProcess :: String -> CreateProcess
pipeOutProcess x =
    CreateProcess
        { cmdspec = ShellCommand x
        , cwd = Nothing
        , env = Nothing
        , std_in = Inherit
        , std_out = CreatePipe
        , std_err = CreatePipe
        , close_fds = False
        , create_group = False
        , delegate_ctlc = False
        , detach_console = False
        , create_new_console = False
        , new_session = False
        , child_group = Nothing
        , child_user = Nothing
        , use_process_jobs = False
        }

runCommandS
    :: String
    -> (Handle -> Handle -> IO a)
    -> IO a
runCommandS s f = do
    withCreateProcess (pipeOutProcess s) g
  where
    g _ (Just out) (Just err) _ = f out err
    g _ _ _ _ = error "runCommandS: unexpected"

data Messages = Messages !Text ![Text]

appendToMessages :: Text -> Messages -> Messages
appendToMessages x (Messages y ys) = Messages (y <> "\n" <> x) ys

newMessage :: Text -> Messages -> Messages
newMessage x (Messages y ys) = Messages x (y : ys)

renderMessages :: Messages -> Text
renderMessages (Messages x xs) = x <> "\n" <> intercalate "\n" xs

accumHandle
    :: ( TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       , MonadHold t m
       , MonadFix m
       )
    => Event t ((String, String), Handle)
    -> m (Dynamic t Text)
accumHandle eh = do
    (e, t) <- newTriggerEvent
    performEvent_ $ ffor eh $ \(cmd, h) -> do
        liftIO $ hSetBuffering h NoBuffering
        liftIO $ hSetEncoding h utf8
        ct <- liftIO getCurrentTime
        liftIO
            $ t
            $ Left
            $ "\n-- "
                <> pack (snd cmd)
                <> " ("
                <> pack (show ct)
                <> ") --\n"
        let loop = do
                hIsEOF h >>= \case
                    True -> return ()
                    False -> do
                        l <- T.hGetLine h
                        t $ Right l
                        loop
        void $ liftIO $ forkIO loop
    msgs <-
        foldDyn
            do
                \case
                    (Left new) -> newMessage new
                    (Right new) -> appendToMessages new
            (Messages mempty [])
            e
    pure $ renderMessages <$> msgs

runCommandSE
    :: (PerformEvent t m, MonadIO (Performable m))
    => Event (t :: Type) (String, String)
    -> m (Event t ((String, String), Handle, Handle))
runCommandSE ecmd = performEvent . ffor ecmd $ \cmd -> do
    (_, mout, merr, _Just) <- liftIO $ createProcess $ pipeOutProcess $ fst cmd
    case (mout, merr) of
        (Just out, Just err) -> return (cmd, out, err)
        _ -> error "runCommandSE: unexpected"

runCommandSED
    :: ( PerformEvent t m
       , MonadIO (Performable m)
       , TriggerEvent t m
       , MonadHold t m
       , MonadFix m
       )
    => Event (t :: Type) (String, String)
    -> m (Dynamic t Text, Dynamic t Text)
runCommandSED ecmd = do
    ehs <- runCommandSE ecmd
    dout <- accumHandle $ outPart <$> ehs
    derr <- accumHandle $ errPart <$> ehs
    return (dout, derr)
  where
    outPart (cmd, out, _err) = (cmd, out)
    errPart (cmd, _out, err) = (cmd, err)
