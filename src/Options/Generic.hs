{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Example use of this library:
--
-- > {-# LANGUAGE DeriveGeneric     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Turtle
-- > import Options.Generic
-- > 
-- > data Example = Go { foo :: Int, bar :: Double, baz :: Int }
-- >     deriving (Generic, Show)
-- > 
-- > instance ParseRecord Example
-- > 
-- > main = do
-- >     x <- options "Test program" parser
-- >     print (x :: Example)
--
-- This produces a program with one sub-command (named @Go@)

module Options.Generic (
    -- * Parsers
      getRecord
    , Only(..)
    , ParseField(..)
    , ParseRecord(..)

    -- * Re-exports
    , Generic
    ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (toUpper)
import Data.Monoid
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Filesystem.Path (FilePath)
import GHC.Generics
import Prelude hiding (FilePath)
import Options.Applicative (Parser, ReadM)

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Typeable
import qualified Filesystem.Path.CurrentOS as Filesystem
import qualified Options.Applicative       as Options
import qualified Options.Applicative.Types as Options
import qualified Text.Read

data Proxy a = Proxy

auto :: Read a => ReadM a
auto = do
    s <- Options.readerAsk
    case Text.Read.readMaybe s of
        Just x  -> return x
        Nothing -> Options.readerAbort Options.ShowHelpText

{-| A class for all types that can be parsed from a single option or argument on
    the command line

    `parseField` has a default implementation for any type that implements
    `Read` and you can derive `Read` for many types

    `metavar` has a default implementation for any type that implements
    `Typeable` and you can derive `Typeable` for any type if you enable the
    @DeriveDataTypeable@ language extension
-}
class ParseField a where
    parseField
        :: Maybe Text
        -- ^ Field label
        -> Parser a
    default parseField :: (Typeable a, Read a) => Maybe Text -> Parser a
    parseField m = do
        let p :: Proxy a
            p = Proxy
        let metavar = map toUpper (show (Data.Typeable.typeRep p))
        case m of
            Nothing   -> do
                let fs =  Options.metavar metavar
                Options.argument auto fs
            Just name -> do
                let fs =  Options.metavar metavar
                       <> Options.long (Data.Text.unpack name)
                Options.option   auto fs

instance ParseField Double
instance ParseField Float
instance ParseField Int
instance ParseField Integer
instance ParseField Ordering
instance ParseField ()
instance ParseField Void

instance ParseField Bool where
    parseField m =
        case m of
            Nothing   -> do
                let fs =  Options.metavar "BOOL"
                Options.argument auto fs
            Just name -> do
                Options.switch (Options.long (Data.Text.unpack name))

instance ParseField Any where
    parseField = fmap (fmap Any) parseField
instance ParseField All where
    parseField = fmap (fmap All) parseField

parseString :: String -> Maybe Text -> Parser String
parseString metavar m =
    case m of
        Nothing   -> do
            let fs = Options.metavar metavar
            Options.argument Options.str fs
        Just name -> do
            let fs =  Options.metavar metavar
                   <> Options.long (Data.Text.unpack name)
            Options.option Options.str fs

instance ParseField Data.Text.Text where
    parseField = fmap (fmap Data.Text.pack) (parseString "TEXT")

instance ParseField Data.Text.Lazy.Text where
    parseField = fmap (fmap Data.Text.Lazy.pack) (parseString "TEXT")

instance ParseField FilePath where
    parseField = fmap (fmap Filesystem.decodeString) (parseString "FILEPATH")

instance ParseField a => ParseField (Maybe a) where
    parseField = fmap optional parseField

instance ParseField a => ParseField [a] where
    parseField = fmap many parseField

{-| Use this to parse a naked field from the command line without wrapping the
    value in a record, like this:

> main = do
>     Only x <- getRecord
>     ...
-}
newtype Only a = Only a deriving (Generic)

instance ParseField a => ParseRecord (Only a)

{-| A class for types that can be parsed from the command line

    This class has a default implementation for any type that implements
    `Generic` and you can derive `Generic` for many types by enabling the
    @DeriveGeneric@ language extension
-}
class ParseRecord a where
    parseRecord :: Parser a
    default parseRecord :: (Generic a, GenericParseRecord (Rep a)) => Parser a
    parseRecord = fmap GHC.Generics.to genericParseRecord

instance ParseField a => ParseRecord (Only a) where
    parseRecord = Options.helper <*> fmap Only (parseField Nothing)

instance ParseRecord Bool
instance ParseRecord Double where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Float where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Int where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Ordering
instance ParseRecord ()

class GenericParseRecord f where
    genericParseRecord :: Parser (f p)

instance GenericParseRecord U1 where
    genericParseRecord = pure U1

instance (GenericParseRecord f, GenericParseRecord g) => GenericParseRecord (f :+: g) where
    genericParseRecord = fmap L1 genericParseRecord <|> fmap R1 genericParseRecord

instance (GenericParseRecord f, GenericParseRecord g) => GenericParseRecord (f :*: g) where
    genericParseRecord = liftA2 (:*:) genericParseRecord genericParseRecord

instance GenericParseRecord V1 where
    genericParseRecord = empty

instance (Selector s, ParseField a) => GenericParseRecord (M1 S s (K1 i a)) where
    genericParseRecord = do
        let m :: M1 i s f a
            m = undefined

        let label = case (selName m) of
                ""   -> Nothing
                name -> Just (Data.Text.pack name)
        fmap (M1 . K1) (parseField label)

instance (Constructor c, GenericParseRecord f) => GenericParseRecord (M1 C c f) where
    genericParseRecord = do
        let m :: M1 i c f a
            m = undefined

        let name = conName m

        let info = Options.info genericParseRecord mempty

        let subparserFields =
                   Options.command name info
                <> Options.metavar name

        let parser = case name of
                "Only" -> genericParseRecord
                _      -> Options.subparser subparserFields

        fmap M1 parser

instance GenericParseRecord f => GenericParseRecord (M1 D c f) where
    genericParseRecord = fmap M1 (Options.helper <*> genericParseRecord)

{-| A brief description of what your program does

    This description will appear in the header of the @--help@ output
-}
newtype Description = Description { getDescription :: Text }
    deriving (IsString)

getRecord :: (MonadIO io, ParseRecord a) => Description -> io a
getRecord desc = liftIO (Options.execParser info)
  where
    header = Options.header (Data.Text.unpack (getDescription desc))

    info = Options.info parseRecord header
