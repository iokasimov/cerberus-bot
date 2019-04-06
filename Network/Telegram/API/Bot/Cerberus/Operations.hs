module Network.Telegram.API.Bot.Cerberus.Operations (registration) where

import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Function ((.), ($))
import "base" System.IO (print)
import "telega" Network.Telegram.API.Bot (Telegram, ask')
import "telega" Network.Telegram.API.Bot.Object (From)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))

import Network.Telegram.API.Bot.Cerberus.Configuration (Environment)
import Network.Telegram.API.Bot.Cerberus.Database (Role, Somebody (Somebody), new)

registration :: From -> Telegram Environment ()
registration from = ask' >>= \(connection, chat_id) -> do
	lift . ExceptT . try $ new from connection
