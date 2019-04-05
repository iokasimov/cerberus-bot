module Network.Telegram.API.Bot.Cerberus.Configuration
	(Environment, Settings (..), settings) where

import "base" Data.Int (Int64)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Control.Applicative ((<*>))
import "base" System.IO (FilePath, IO)
import "base" Prelude (negate)
import "optparse-applicative" Options.Applicative (Parser
	, execParser, argument, auto, info, fullDesc, metavar, str)
import "sqlite-simple" Database.SQLite.Simple (Connection, open)
import "telega" Network.Telegram.API.Bot (Token (Token))
import "text" Data.Text (pack)
import "wreq" Network.Wreq.Session (Session, newAPISession)

type Environment = (Connection, Int64)

data Arguments = Arguments Token Int64 FilePath

options :: Parser Arguments
options = Arguments <$> token <*> chat_id <*> db_filepath where

	token :: Parser Token
	token = Token . pack <$> argument str (metavar "TELEGRAM_TOKEN")

	chat_id :: Parser Int64
	chat_id = negate <$> argument auto (metavar "CHAT_ID")

	db_filepath :: Parser FilePath
	db_filepath = argument str (metavar "DB_FILEPATH")

data Settings = Settings Token Int64 Connection Session

settings :: IO Settings
settings = do
	Arguments token chat_id db_filepath <- execParser $ info options fullDesc
	Settings token chat_id <$> open db_filepath <*> newAPISession
