module Network.Telegram.API.Bot.Cerberus.Database (Role (..)) where

import "base" Control.Applicative (Applicative (pure, (<*>)))
import "base" Data.Function (id, ($), (&))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Maybe (maybe)
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" System.IO (IO)
import "base" Text.Show (Show (show))
import "lens" Control.Lens (element, (^?))
import "sqlite-simple" Database.SQLite.Simple (Connection, Only (Only), SQLData (SQLInteger), query)
import "sqlite-simple" Database.SQLite.Simple.Ok (Ok (Ok))
import "sqlite-simple" Database.SQLite.Simple.Internal (Field (Field))
import "sqlite-simple" Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), returnError)
import "sqlite-simple" Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import "sqlite-simple" Database.SQLite.Simple.ToField (ToField (toField))
import "sqlite-simple" Database.SQLite.Simple.ToRow (ToRow (toRow))
import "telega" Network.Telegram.API.Bot.Object.From (From)
import "telega" Network.Telegram.API.Bot.Property.Identifiable (Identifiable (identificator))

data Role = Guest | Partaker deriving Show

instance FromField Role where
	fromField (Field (SQLInteger 0) _) = Ok Guest
	fromField (Field (SQLInteger 1) _) = Ok Partaker
	fromField f@(Field (SQLInteger x) _) = returnError ConversionFailed f
		("Role identificator must be 0 or 1, got " <> show x)
	fromField f = returnError ConversionFailed f "expecting an SQLInteger column type"

instance ToField Role where
	toField Guest = SQLInteger 0
	toField Partaker = SQLInteger 1

data Somebody = Somebody Int Role deriving Show

instance FromRow Somebody where
	fromRow = Somebody <$> field <*> field

instance ToRow Somebody where
	toRow (Somebody tlg_id role) = toRow (tlg_id, role)

check :: From -> Connection -> IO Somebody
check from connection = do
	r <- query connection "SELECT * from members where id = ?" (Only $ identificator from)
	pure $ r ^? element 0 & maybe (Somebody (identificator from) Guest) id
