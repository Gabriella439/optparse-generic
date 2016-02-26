{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
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
    , ParseField(..)
    , ParseRecord(..)

    -- * Re-exports
    , Generic
    ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO(..))
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
import qualified Data.Typeable
import qualified Filesystem.Path.CurrentOS
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
    parseField :: ReadM a
    default parseField :: Read a => ReadM a
    parseField = auto

    metavar :: proxy a -> Text
    default metavar :: Typeable a => proxy a -> Text
    metavar proxy = Data.Text.pack (show (Data.Typeable.typeRep proxy))

instance ParseField Integer

instance ParseField Bool
instance ParseField Char
instance ParseField Double
instance ParseField Float
instance ParseField Int
instance ParseField Ordering
instance ParseField ()
instance ParseField Any where
    parseField = fmap Any parseField
    metavar  _ = "Bool"
instance ParseField All where
    parseField = fmap All parseField
    metavar  _ = "Bool"
instance ParseField Void
instance ParseField Text where
    parseField = fmap Data.Text.pack Options.str
instance ParseField FilePath where
    parseField = fmap Filesystem.Path.CurrentOS.decodeString Options.str

newtype Unnamed a = Unnamed { getUnnamed :: a }

{-| A class for types that can be parsed from the command line

    This class has a default implementation for any type that implements
    `Generic` and you can derive `Generic` for many types by enabling the
    @DeriveGeneric@ language extension
-}
class ParseRecord a where
    parseRecord :: Parser a
    default parseRecord :: (Generic a, GenericParseRecord (Rep a)) => Parser a
    parseRecord = fmap GHC.Generics.to genericParseRecord

instance ParseField a => ParseRecord (Unnamed a) where
    parseRecord = Options.helper <*> fmap Unnamed p
      where
        p = Options.argument parseField
                (Options.metavar (Data.Text.unpack (metavar p)))

-- TODO: Why is there no `Generic` instance for `Integer`?
instance ParseRecord Bool
instance ParseRecord Char where
    parseRecord = fmap getUnnamed parseRecord
instance ParseRecord Double where
    parseRecord = fmap getUnnamed parseRecord
instance ParseRecord Float where
    parseRecord = fmap getUnnamed parseRecord
instance ParseRecord Int where
    parseRecord = fmap getUnnamed parseRecord
instance ParseRecord Ordering
instance ParseRecord ()

-- TODO: Add flag names
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

        let p :: Proxy a
            p = Proxy

        let name = selName m

        let parser = case name of
                "" ->
                    Options.argument parseField
                        (Options.metavar (Data.Text.unpack (metavar p)))
                         
                _  ->
                    Options.option parseField
                        (   Options.metavar (Data.Text.unpack (metavar p))
                        <>  Options.long name
                        )
        fmap (M1 . K1) parser

instance (Constructor c, GenericParseRecord f) => GenericParseRecord (M1 C c f) where
    genericParseRecord = do
        let m :: M1 i c f a
            m = undefined

        let name = conName m

        let info = Options.info genericParseRecord mempty

        let subparserFields =
                   Options.command name info
                <> Options.metavar name

        fmap M1 (Options.subparser subparserFields)

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
