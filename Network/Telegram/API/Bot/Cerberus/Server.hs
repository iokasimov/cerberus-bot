{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Telegram.API.Bot.Cerberus.Server (API, server) where

import "async" Control.Concurrent.Async (async)
import "base" Control.Applicative (pure)
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Bool ((||))
import "base" Data.Eq ((/=))
import "base" Data.Function ((.), ($))
import "base" Data.Functor (void)
import "base" System.IO (print)
import "lens" Control.Lens ((^.))
import "servant-server" Servant (Capture, ReqBody, Server, JSON, Post, FromHttpApiData, ToHttpApiData, type (:>), err403, throwError)
import "telega" Network.Telegram.API.Bot (Telegram, Token (Token), telegram)
import "telega" Network.Telegram.API.Bot.Property (identificator)
import "telega" Network.Telegram.API.Bot.Object.Message (Message (Textual))
import "telega" Network.Telegram.API.Bot.Object.Update (Update (Membership, Incoming), chat)
import "transformers" Control.Monad.Trans.Class (lift)

import Network.Telegram.API.Bot.Cerberus.Configuration (Environment, Settings (Settings))

type API = "webhook" :> Capture "secret" Token :> ReqBody '[JSON] Update :> Post '[JSON] ()

deriving instance ToHttpApiData Token
deriving instance FromHttpApiData Token

server :: Settings -> Server API
server (Settings token chat_id connection session) secret update =
	if secret /= token || (identificator $ update ^. chat) /= chat_id then throwError err403 else
		liftIO . void . async . telegram session token (connection, chat_id) $ webhook update

webhook :: Update -> Telegram Environment ()
webhook i@(Incoming _ (Textual _ _ _ _)) = lift . lift . print $ i
webhook (Membership _ m) = lift . lift . print $ m
webhook x = lift . lift . print $ x
