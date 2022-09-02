{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | This library auto-generates command-line parsers for data types using
-- Haskell's built-in support for generic programming.  The best way to
-- understand how this library works is to walk through a few examples.
--
-- For example, suppose that you want to parse a record with named fields like
-- this:
--
-- > -- Example.hs
-- >
-- > {-# LANGUAGE DeriveGeneric     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Options.Generic
-- > 
-- > data Example = Example { foo :: Int, bar :: Double }
-- >     deriving (Generic, Show)
-- > 
-- > instance ParseRecord Example
-- > 
-- > main = do
-- >     x <- getRecord "Test program"
-- >     print (x :: Example)
--
-- Named fields translate to flags which you can provide in any order:
--
-- > $ stack build optparse-generic
-- > $ stack runghc Example.hs -- --bar 2.5 --foo 1
-- > Example {foo = 1, bar = 2.5}
--
-- This also auto-generates @--help@ output:
--
-- > $ stack runghc Example.hs -- --help
-- > Test program
-- > 
-- > Usage: Example.hs --foo INT --bar DOUBLE
-- > 
-- > Available options:
-- >   -h,--help                Show this help text
--
-- You can also add help descriptions to each field, like this:
--
-- > {-# LANGUAGE DataKinds         #-}
-- > {-# LANGUAGE DeriveGeneric     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE TypeOperators     #-}
-- > 
-- > import Options.Generic
-- > 
-- > data Example = Example
-- >     { foo :: Int    <?> "Documentation for the foo flag"
-- >     , bar :: Double <?> "Documentation for the bar flag"
-- >     } deriving (Generic, Show)
-- > 
-- > instance ParseRecord Example
-- > 
-- > main = do
-- >     x <- getRecord "Test program"
-- >     print (x :: Example)
--
-- ... which produces the following @--help@ output:
--
-- > $ stack runghc Example.hs -- --help
-- > Test program
-- > 
-- > Usage: Example.hs --foo INT --bar DOUBLE
-- > 
-- > Available options:
-- >   -h,--help                Show this help text
-- >   --foo INT                Documentation for the foo flag
-- >   --bar DOUBLE             Documentation for the bar flag
--
-- However, any fields you document will be wrapped in the `Helpful`
-- constructor:
--
-- > $ stack runghc Example.hs -- --foo 1 --bar 2.5
-- > Example {foo = Helpful {unHelpful = 1}, bar = Helpful {unHelpful = 2.5}}
--
-- To avoid this, while still being able to document your fields, you may
-- generalize the definition of your record with a parameter 'w', and use
-- 'unwrapRecord'.
--
-- > {-# LANGUAGE DataKinds          #-}
-- > {-# LANGUAGE DeriveGeneric      #-}
-- > {-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
-- > {-# LANGUAGE OverloadedStrings  #-}
-- > {-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
-- > {-# LANGUAGE TypeOperators      #-}
-- >
-- > import Options.Generic
-- >
-- > data Example w = Example
-- >     { foo :: w ::: Int    <?> "Documentation for the foo flag"
-- >     , bar :: w ::: Double <?> "Documentation for the bar flag"
-- >     } deriving (Generic)
-- >
-- > instance ParseRecord (Example Wrapped)
-- > deriving instance Show (Example Unwrapped)
-- >
-- > main = do
-- >     x <- unwrapRecord "Test program"
-- >     print (x :: Example Unwrapped)
--
-- @Example Unwrapped@ is equivalent to a record type with simple fields:
--
-- > $ stack runghc Example.hs -- --foo 1 --bar 2.5
-- > Example {foo = 1, bar = 2.5}
--
-- You can also add default values to each `Read`able field, like this:
--
-- > {-# LANGUAGE DataKinds         #-}
-- > {-# LANGUAGE DeriveGeneric     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE TypeOperators     #-}
-- > 
-- > import Options.Generic
-- > 
-- > data Example = Example
-- >     { foo :: Int    <!> "1"
-- >     , bar :: String <!> "hello"
-- >     } deriving (Generic, Show)
-- > 
-- > instance ParseRecord Example
-- > 
-- > main = do
-- >     x <- getRecord "Test program"
-- >     print (x :: Example)
--
-- Default values will work alongside help descriptions and unwrapping.
--
-- For the following examples I encourage you to test what @--help@ output they
-- generate.
--
-- This library will also do the right thing if the fields have no labels:
--
-- > data Example = Example Int Double deriving (Generic, Show)
--
-- Fields without labels translate into positional command-line arguments:
--
-- > $ stack runghc Example.hs -- 1 2.5
-- > Example 1 2.5
--
-- Certain types of fields are given special treatment, such as in this
-- example:
--
-- > data Example = Example
-- >     { switch   :: Bool
-- >     , list     :: [Int]
-- >     , optional :: Maybe   Int
-- >     , first    :: First   Int
-- >     , last     :: Last    Int
-- >     , sum      :: Sum     Int
-- >     , product  :: Product Int
-- >     } deriving (Generic, Show)
--
-- This gives the following behavior:
--
-- > $ stack runghc Example.hs --
-- >       --switch
-- >       --optional 1
-- >       --list    1 --list    2
-- >       --first   1 --first   2
-- >       --last    1 --last    2
-- >       --sum     1 --sum     2
-- >       --product 1 --product 2
-- > Example {switch = True, list = [1,2], optional = Just 1, first = First 
-- > {getFirst = Just 1}, last = Last {getLast = Just 2}, sum = Sum {getSum =
-- > 3}, product = Product {getProduct = 2}}
-- > 
-- > $ stack runghc Example.hs
-- > Example {switch = False, list = [], optional = Nothing, first = First
-- > {getFirst = Nothing}, second = Last {getLast = Nothing}, sum = Sum {getSum
-- > = 0}, product = Product {getProduct = 1}}
--
-- If a datatype has multiple constructors:
--
-- > data Example
-- >     = Create { name :: Text, duration :: Maybe Int }
-- >     | Kill   { name :: Text }
-- >     deriving (Generic, Show)
--
-- ... then they will translate into subcommands named after each constructor:
--
-- > $ stack runghc Example.hs -- create --name foo --duration=60
-- > Create {name = "foo", duration = Just 60}
-- > $ stack runghc Example.hs -- kill --name foo
-- > Kill {name = "foo"}
--
-- This library also provides out-of-the-box support for many existing types,
-- like tuples and `Either`.
--
-- > {-# LANGUAGE DeriveGeneric     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Options.Generic
-- > 
-- > main = do
-- >     x <- getRecord "Test program"
-- >     print (x :: Either Double Int)
--
-- > $ stack runghc Example.hs -- left 1.0
-- > Left 1.0
-- > $ stack runghc Example.hs -- right 2
-- > Right 2
-- 
-- > main = do
-- >     x <- getRecord "Test program"
-- >     print (x :: (Double, Int))
--
-- > $ stack runghc Example.hs -- 1.0 2
-- > (1.0,2)
--
-- ... and you can also just parse a single value:
--
-- > main = do
-- >     x <- getRecord "Test program"
-- >     print (x :: Int)
--
-- > $ stack runghc Example.hs -- 2
-- > 2
--
-- However, there are some types that this library cannot generate sensible
-- command-line parsers for, such as:
--
-- * recursive types:
--
--     > data Example = Example { foo :: Example }
--
-- * records whose fields are other records
--
--     > data Outer = Outer { foo :: Inner } deriving (Show, Generic)
--     > data Inner = Inner { bar :: Int   } deriving (Show, Generic)
--
-- * record fields  with nested `Maybe`s or nested lists
--
--     > data Example = Example { foo :: Maybe (Maybe Int) }
--     > data Example = Example { foo :: [[Int]]           }
--
-- If you try to auto-generate a parser for these types you will get an error at
-- compile time that will look something like this:
--
-- >     No instance for (ParseFields TheTypeOfYourField)
-- >       arising from a use of ‘Options.Generic.$gdmparseRecord’
-- >     In the expression: Options.Generic.$gdmparseRecord
-- >     In an equation for ‘parseRecord’:
-- >         parseRecord = Options.Generic.$gdmparseRecord
-- >     In the instance declaration for ‘ParseRecord TheTypeOfYourRecord’
--
-- You can customize the library's default behavior using the
-- `parseRecordWithModifiers` utility, like this:
--
-- > {-# LANGUAGE DeriveGeneric     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Options.Generic
-- > 
-- > data Example = Example { foo :: Int, bar :: Double }
-- >     deriving (Generic, Show)
-- > 
-- > modifiers :: Modifiers
-- > modifiers = defaultModifiers
-- >     { shortNameModifier = firstLetter
-- >     }
-- >
-- > instance ParseRecord Example where
-- >     parseRecord = parseRecordWithModifiers modifiers
-- > 
-- > main = do
-- >     x <- getRecord "Test program"
-- >     print (x :: Example)

module Options.Generic (
    -- * Parsers
      getRecord
    , getRecordWith
    , getWithHelpWith
    , getWithHelp
    , getRecordPure
    , getRecordPureWith
    , unwrapRecord
    , unwrapWithHelp
    , unwrapRecordPure
    , unwrap
    , ParseRecord(..)
    , ParseFields(..)
    , ParseField(..)
    , Only(..)
    , getOnly
    , readIntegralBounded
    , Modifiers(..)
    , parseRecordWithModifiers
    , defaultModifiers
    , lispCaseModifiers
    , firstLetter
    , GenericParseRecord(..)

    -- * Help
    , type (<?>)(..)
    , type (<!>)(..)
    , type (<#>)(..)
    , type (:::)
    , Wrapped
    , Unwrapped
    , Unwrappable

    -- * Re-exports
    , Generic
    , Text
    , All(..)
    , Any(..)
    , First(..)
    , Last(..)
    , Sum(..)
    , Product(..)
    ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Char (isUpper, toLower, toUpper)
import Data.Data (Data)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy
import Data.Text (Text)
import Data.Tuple.Only (Only(..))
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Foldable (foldMap)
import Filesystem.Path (FilePath)
import GHC.Generics
import Prelude hiding (FilePath)
import Options.Applicative (Parser, ReadM)

import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Time.Calendar
import qualified Data.Time.Format
import qualified Data.Typeable
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Filesystem.Path.CurrentOS    as Filesystem
import qualified Options.Applicative          as Options
import qualified Options.Applicative.Types    as Options
import qualified Options.Applicative.NonEmpty as Options.NonEmpty
import qualified Text.Read

#if MIN_VERSION_base(4,7,0)
import GHC.TypeLits
#else
import Data.Singletons.TypeLits
#endif

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural (Natural)
#endif

auto :: Read a => ReadM a
auto = do
    s <- Options.readerAsk
    case Text.Read.readMaybe s of
        Just x  -> return x
        Nothing -> Options.readerAbort (Options.ShowHelpText Nothing)

{-| A class for all record fields that can be parsed from exactly one option or
    argument on the command line

    `parseField` has a default implementation for any type that implements
    `Read` and `Typeable`.  You can derive `Read` for many types and you can
    derive `Typeable` for any type if you enable the @DeriveDataTypeable@
    language extension
-}
class ParseField a where
    parseField
        :: Maybe Text
        -- ^ Help message
        -> Maybe Text
        -- ^ Field label
        -> Maybe Char
        -- ^ Short name
        -> Maybe String
        -- ^ Default value
        -> Parser a
    default parseField
        :: Maybe Text
        -- ^ Help message
        -> Maybe Text
        -- ^ Field label
        -> Maybe Char
        -- ^ Short name
        -> Maybe String
        -- ^ Default value
        -> Parser a
    parseField h m c d = do
        let proxy = Proxy :: Proxy a
        case m of
            Nothing   -> do
                let fs =  Options.metavar (metavar proxy)
                       <> foldMap (Options.help . Data.Text.unpack) h
                Options.argument readField fs
            Just name -> do
                let fs =  Options.metavar (metavar proxy)
                       <> Options.long (Data.Text.unpack name)
                       <> foldMap (Options.help . Data.Text.unpack) h
                       <> foldMap Options.short c
                       <> foldMap Options.value (d >>= runReadM readField)
                       <> foldMap (Options.showDefaultWith . const) d
                Options.option   readField fs

    {-| The only reason for this method is to provide a special case for
        handling `String`s.  All other instances should just fall back on the
        default implementation for `parseListOfField`
    -}
    parseListOfField
        :: Maybe Text
        -- ^ Help message
        -> Maybe Text
        -- ^ Field label
        -> Maybe Char
        -- ^ Short name
        -> Maybe String
        -- ^ Default value
        -> Parser [a]
    parseListOfField h m c d = many (parseField h m c d)

    readField :: ReadM a
    default readField :: Read a => ReadM a
    readField = auto

    metavar :: proxy a -> String
    default metavar :: Typeable a => proxy a -> String
    metavar _ = map toUpper (show (Data.Typeable.typeOf (undefined :: a)))

-- | a readMaybe using provided ReadM
runReadM :: ReadM a -> String -> Maybe a
runReadM r s = either (const Nothing) Just $
    runExcept (runReaderT (Options.unReadM r) s)

instance ParseField Bool
instance ParseField Double
instance ParseField Float
instance ParseField Integer
instance ParseField Ordering
instance ParseField ()
instance ParseField Void

readIntegralBounded :: forall a. (Integral a, Bounded a, Typeable a, ParseField a) => ReadM a
readIntegralBounded =
    auto >>= f
    where
        f i | i < lower = fail msg
            | i > upper = fail msg
            | otherwise = pure $ fromInteger i
        lower = toInteger (minBound :: a)
        upper = toInteger (maxBound :: a)
        msg = metavar (Proxy :: Proxy a) <>
              " must be within the range [" <>
              show lower <> " .. " <> show upper <> "]"

instance ParseField Int    where readField = readIntegralBounded
instance ParseField Int8   where readField = readIntegralBounded
instance ParseField Int16  where readField = readIntegralBounded
instance ParseField Int32  where readField = readIntegralBounded
instance ParseField Int64  where readField = readIntegralBounded
instance ParseField Word8  where readField = readIntegralBounded
instance ParseField Word16 where readField = readIntegralBounded
instance ParseField Word32 where readField = readIntegralBounded
instance ParseField Word64 where readField = readIntegralBounded

#if MIN_VERSION_base(4,8,0)
instance ParseField Natural where
    readField =
        auto >>= f
        where
            f i | i < 0 = fail msg
                | otherwise = pure $ fromInteger i
            msg = "NATURAL cannot be negative"
#endif

instance ParseField String where
    parseField = parseHelpfulString "STRING"

instance ParseField Char where
    metavar _ = "CHAR"
    readField = do
        s <- Options.readerAsk
        case s of
            [ch] -> return ch
            _    -> Options.readerAbort (Options.ShowHelpText Nothing)

    parseListOfField = parseHelpfulString "STRING"

instance ParseField Any where
    metavar _ = "ANY"
    parseField h m c d = Any <$> parseField h m c d
instance ParseField All where
    metavar _ = "ALL"
    parseField h m c d = All <$> parseField h m c d

parseHelpfulString
    :: String -> Maybe Text -> Maybe Text -> Maybe Char -> Maybe String -> Parser String
parseHelpfulString metavar h m c d =
    case m of
        Nothing   -> do
            let fs =  Options.metavar metavar
                   <> foldMap (Options.help . Data.Text.unpack) h
            Options.argument Options.str fs
        Just name -> do
            let fs =  Options.metavar metavar
                   <> Options.long (Data.Text.unpack name)
                   <> foldMap (Options.help . Data.Text.unpack) h
                   <> foldMap Options.short c
                   <> foldMap ((Options.showDefault <>) . Options.value) d
            Options.option Options.str fs

instance ParseField Data.Text.Text where
    parseField h m c d = Data.Text.pack <$> parseHelpfulString "TEXT" h m c d

instance ParseField Data.ByteString.ByteString where
    parseField h m c d = fmap Data.Text.Encoding.encodeUtf8 (parseField h m c d)

instance ParseField Data.Text.Lazy.Text where
    parseField h m c d = Data.Text.Lazy.pack <$> parseHelpfulString "TEXT" h m c d

instance ParseField Data.ByteString.Lazy.ByteString where
    parseField h m c d = fmap Data.Text.Lazy.Encoding.encodeUtf8 (parseField h m c d)

instance ParseField FilePath where
    parseField h m c d = Filesystem.decodeString <$> parseHelpfulString "FILEPATH" h m c d
    readField = Options.str

instance ParseField Data.Time.Calendar.Day where
    metavar _ = "YYYY-MM-DD"
    readField = Options.eitherReader
              $ runReadS . Data.Time.Format.readSTime
                            False
                            Data.Time.Format.defaultTimeLocale
                            "%F"
        where
            runReadS [(day, "")] = Right day
            runReadS _           = Left "expected YYYY-MM-DD"

{-| A class for all types that can be parsed from zero or more arguments/options
    on the command line

    `parseFields` has a default implementation for any type that implements
    `ParseField`
-}
class ParseRecord a => ParseFields a where
    parseFields
        :: Maybe Text
        -- ^ Help message
        -> Maybe Text
        -- ^ Field label
        -> Maybe Char
        -- ^ Short name
        -> Maybe String
        -- ^ Default value
        -> Parser a
    default parseFields
        :: ParseField a => Maybe Text -> Maybe Text -> Maybe Char -> Maybe String -> Parser a
    parseFields = parseField

instance ParseFields Char
instance ParseFields Double
instance ParseFields Float
instance ParseFields Int
instance ParseFields Int8
instance ParseFields Int16
instance ParseFields Int32
instance ParseFields Int64
instance ParseFields Integer
instance ParseFields Ordering
instance ParseFields Void
instance ParseFields Word8
instance ParseFields Word16
instance ParseFields Word32
instance ParseFields Word64
instance ParseFields Data.ByteString.ByteString
instance ParseFields Data.ByteString.Lazy.ByteString
instance ParseFields Data.Text.Text
instance ParseFields Data.Text.Lazy.Text
instance ParseFields FilePath
instance ParseFields Data.Time.Calendar.Day

#if MIN_VERSION_base(4,8,0)
instance ParseFields Natural
#endif

instance ParseFields Bool where
    parseFields h m c d =
        case m of
            Nothing   -> do
                let fs =  Options.metavar "BOOL"
                       <> foldMap (Options.help . Data.Text.unpack) h
                Options.argument auto fs
            Just name -> case d >>= Text.Read.readMaybe of
                Nothing -> Options.switch $
                  Options.long (Data.Text.unpack name)
                  <> foldMap (Options.help . Data.Text.unpack) h
                  <> foldMap Options.short c
                Just d0 -> Options.flag d0 (not d0) $
                  Options.long (Data.Text.unpack name)
                  <> foldMap (Options.help . Data.Text.unpack) h
                  <> foldMap Options.short c
                 

instance ParseFields () where
    parseFields _ _ _ _ = pure ()

instance ParseFields Any where
    parseFields h m c d = (fmap mconcat . many . fmap Any) (parseField h m c d)

instance ParseFields All where
    parseFields h m c d = (fmap mconcat . many . fmap All) (parseField h m c d)

instance ParseField a => ParseFields (Maybe a) where
    parseFields h m c d = optional (parseField h m c d)

instance ParseField a => ParseFields (First a) where
    parseFields h m c d = (fmap mconcat . many . fmap (First . Just)) (parseField h m c d)

instance ParseField a => ParseFields (Last a) where
    parseFields h m c d = (fmap mconcat . many . fmap (Last . Just)) (parseField h m c d)

instance (Num a, ParseField a) => ParseFields (Sum a) where
    parseFields h m c d = (fmap mconcat . many . fmap Sum) (parseField h m c d)

instance (Num a, ParseField a) => ParseFields (Product a) where
    parseFields h m c d = (fmap mconcat . many . fmap Product) (parseField h m c d)

instance ParseField a => ParseFields [a] where
    parseFields = parseListOfField

instance ParseField a => ParseFields (NonEmpty a) where
    parseFields h m c d = Options.NonEmpty.some1 (parseField h m c d)

{-| Use this to annotate a field with a type-level string (i.e. a `Symbol`)
    representing the help description for that field:

> data Example = Example
>     { foo :: Int    <?> "Documentation for the foo flag"
>     , bar :: Double <?> "Documentation for the bar flag"
>     } deriving (Generic, Show)
-}
newtype (<?>) (field :: *) (help :: Symbol) = Helpful { unHelpful :: field } deriving (Generic, Show, Data)

instance (ParseField a, KnownSymbol h) => ParseField (a <?> h) where
    parseField _ m c d = Helpful <$>
      parseField ((Just . Data.Text.pack .symbolVal) (Proxy :: Proxy h)) m c d
    readField = Helpful <$> readField
    metavar _ = metavar (Proxy :: Proxy a)

instance (ParseFields a, KnownSymbol h) => ParseFields (a <?> h) where
    parseFields _ m c d = Helpful <$>
      parseFields ((Just . Data.Text.pack .symbolVal) (Proxy :: Proxy h)) m c d
instance (ParseFields a, KnownSymbol h) => ParseRecord (a <?> h)

{-| Use this to annotate a field with a type-level string (i.e. a `Symbol`)
    representing the default value for that field:

> data Example = Example
>     { foo :: Int    <!> "1"
>     , bar :: Double <!> "0.5"
>     } deriving (Generic, Show)
-}
newtype (<!>) (field :: *) (value :: Symbol) = DefValue { unDefValue :: field } deriving (Generic, Show, Data)

instance (ParseField a, KnownSymbol d) => ParseField (a <!> d) where
    parseField h m c _ = DefValue <$> parseField h m c (Just (symbolVal (Proxy :: Proxy d)))
    readField = DefValue <$> readField
    metavar _ = metavar (Proxy :: Proxy a)

instance (ParseFields a, KnownSymbol d) => ParseFields (a <!> d) where
    parseFields h m c _ = DefValue <$> parseFields h m c (Just (symbolVal (Proxy :: Proxy d)))
instance (ParseFields a, KnownSymbol h) => ParseRecord (a <!> h)

{-| Use this to annotate a field with a type-level char (i.e. a `Symbol`)
    representing the short name of the field (only the first character of the
    symbol is used):

> data Example = Example
>     { foo :: Int    <#> "f"
>     , bar :: Double <#> "b"
>     } deriving (Generic, Show)
-}
newtype (<#>) (field :: *) (value :: Symbol) = ShortName { unShortName :: field } deriving (Generic, Show, Data)

instance (ParseField a, KnownSymbol c) => ParseField (a <#> c) where
    parseField h m _ d = ShortName <$> parseField h m (listToMaybe (symbolVal (Proxy :: Proxy c))) d
    readField = ShortName <$> readField
    metavar _ = metavar (Proxy :: Proxy a)

instance (ParseFields a, KnownSymbol c) => ParseFields (a <#> c) where
    parseFields h m _ d = ShortName <$> parseFields h m (listToMaybe (symbolVal (Proxy :: Proxy c))) d
instance (ParseFields a, KnownSymbol h) => ParseRecord (a <#> h)

{-| A 1-tuple, used solely to translate `ParseFields` instances into
    `ParseRecord` instances
-}
newtype Only_ a = Only_ a deriving (Generic, Show)

{-| This is a convenience function that you can use if you want to create a
    `ParseRecord` instance that just defers to the `ParseFields` instance for
    the same type:

> instance ParseRecord MyType where
>     parseRecord = fmap getOnly parseRecord
-}
getOnly :: Only a -> a
getOnly (Only x) = x

{-| A class for types that can be parsed from the command line

    This class has a default implementation for any type that implements
    `Generic` and you can derive `Generic` for many types by enabling the
    @DeriveGeneric@ language extension

    You can also use `getOnly` to create a `ParseRecord` instance from a
    `ParseFields` instance:

> instance ParseRecord MyType where
>     parseRecord = fmap getOnly parseRecord
-}
class ParseRecord a where
    parseRecord :: Parser a
    default parseRecord :: (Generic a, GenericParseRecord (Rep a)) => Parser a
    parseRecord = fmap GHC.Generics.to (genericParseRecord defaultModifiers)

instance ParseFields a => ParseRecord (Only_ a)
instance ParseFields a => ParseRecord (Only a) where
    parseRecord = fmap adapt parseRecord
      where
        adapt (Only_ x) = Only x

instance ParseRecord Char where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Double where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Float where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Int where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Int8 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Int16 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Int32 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Int64 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Ordering
instance ParseRecord Void
instance ParseRecord Word8 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Word16 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Word32 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Word64 where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord ()

#if MIN_VERSION_base(4,8,0)
instance ParseRecord Natural where
    parseRecord = fmap getOnly parseRecord
#endif

instance ParseRecord Bool where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord Integer where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord Data.Text.Text where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord Data.Text.Lazy.Text where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord Any where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord All where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord FilePath where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord Data.ByteString.ByteString where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord Data.ByteString.Lazy.ByteString where
    parseRecord = fmap getOnly parseRecord

instance ParseRecord Data.Time.Calendar.Day where
    parseRecord = fmap getOnly parseRecord

instance ParseField a => ParseRecord (Maybe a) where
    parseRecord = fmap getOnly parseRecord

instance ParseField a => ParseRecord (First a) where
    parseRecord = fmap getOnly parseRecord

instance ParseField a => ParseRecord (Last a) where
    parseRecord = fmap getOnly parseRecord

instance (Num a, ParseField a) => ParseRecord (Sum a) where
    parseRecord = fmap getOnly parseRecord

instance (Num a, ParseField a) => ParseRecord (Product a) where
    parseRecord = fmap getOnly parseRecord

instance ParseField a => ParseRecord [a] where
    parseRecord = fmap getOnly parseRecord

instance ParseField a => ParseRecord (NonEmpty a) where
    parseRecord = fmap getOnly parseRecord

instance (ParseFields a, ParseFields b) => ParseRecord (a, b)
instance (ParseFields a, ParseFields b, ParseFields c) => ParseRecord (a, b, c)
instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d) => ParseRecord (a, b, c, d)
instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d, ParseFields e) => ParseRecord (a, b, c, d, e)
instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d, ParseFields e, ParseFields f) => ParseRecord (a, b, c, d, e, f)
instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d, ParseFields e, ParseFields f, ParseFields g) => ParseRecord (a, b, c, d, e, f, g)

instance (ParseFields a, ParseFields b) => ParseRecord (Either a b)

{-| Options for customizing derived `ParseRecord` implementations for `Generic`
    types

    You can either create the `Modifiers` record directly:

    > modifiers :: Modifiers
    > modifiers = Modifiers
    >     { fieldNameModifier       = ...
    >     , constructorNameModifier = ...
    >     , shortNameModifier       = ...
    >     }

    ... or you can tweak the `defaultModifiers`:

    > modifiers :: Modifiers
    > modifiers = defaultModifiers { fieldNameModifier = ... }

    ... or you can use/tweak a predefined `Modifier`, like `lispCaseModifiers`

    The `parseRecordWithModifiers` function uses this `Modifiers` record when
    generating a `Generic` implementation of `ParseRecord`
-}
data Modifiers = Modifiers
  { fieldNameModifier :: String -> String
  -- ^ Transform the name of derived fields (Default: @id@)
  , constructorNameModifier :: String -> String
  -- ^ Transform the name of derived constructors (Default: @map toLower@)
  , shortNameModifier :: String -> Maybe Char
  -- ^ Derives an optional short name from the field name (Default: @\\_ -> Nothing@)
  }

{-| These are the default modifiers used if you derive a `Generic`
    implementation.  You can customize this and pass the result to
    `parseRecordWithModifiers` if you would like to modify the derived
    implementation:

    > myModifiers :: Modifiers
    > myModifiers = defaultModifiers { constructorNameModifier = id }
    >
    > instance ParseRecord MyType where
    >     parseRecord = parseRecordWithModifiers myModifiers
-}
defaultModifiers :: Modifiers
defaultModifiers = Modifiers
    { fieldNameModifier       = id
    , constructorNameModifier = map toLower
    , shortNameModifier       = \_ -> Nothing
    }

-- | Convert field and constructor names from @CamelCase@ to @lisp-case@.
--
-- Leading underscores are dropped, allowing one to use option names
-- which are Haskell keywords or otherwise conflicting identifiers.
--
-- > BuildCommand -> build-command
-- > someFlag -> --some-flag
-- > _type -> --type
-- > _splitAt -> --split-at
lispCaseModifiers :: Modifiers
lispCaseModifiers = Modifiers lispCase lispCase (\_ -> Nothing)
  where
    lispCase = dropWhile (== '-') . (>>= lower) . dropWhile (== '_')
    lower c | isUpper c = ['-', toLower c]
            | otherwise = [c]

{-| Use this for the `shortNameModifier` field of the `Modifiers` record if
    you want to use the first letter of each option as the short name
-}
firstLetter :: String -> Maybe Char
firstLetter (c:_) = Just c
firstLetter  _    = Nothing

class GenericParseRecord f where
    genericParseRecord :: Modifiers -> Parser (f p)

instance GenericParseRecord U1 where
    genericParseRecord _ = pure U1

-- See: [NOTE - Sums]
instance GenericParseRecord f => GenericParseRecord (M1 C c f) where
    genericParseRecord = fmap M1 . genericParseRecord

-- See: [NOTE - Sums]
instance (GenericParseRecord (f :+: g), GenericParseRecord (h :+: i)) => GenericParseRecord ((f :+: g) :+: (h :+: i)) where
    genericParseRecord mods = do
        fmap L1 (genericParseRecord mods) <|> fmap R1 (genericParseRecord mods)

-- See: [NOTE - Sums]
instance (Constructor c, GenericParseRecord f, GenericParseRecord (g :+: h)) => GenericParseRecord (M1 C c f :+: (g :+: h)) where
    genericParseRecord mods@Modifiers{..} = do
        let m :: M1 i c f a
            m = undefined

        let name = constructorNameModifier (conName m)

        let info = Options.info (Options.helper <*> (genericParseRecord mods)) mempty

        let subparserFields =
                   Options.command name info
                <> Options.metavar name

        let parser = Options.subparser subparserFields

        fmap (L1 . M1) parser <|> fmap R1 (genericParseRecord mods)

-- See: [NOTE - Sums]
instance (Constructor c, GenericParseRecord (f :+: g), GenericParseRecord h) => GenericParseRecord ((f :+: g) :+: M1 C c h) where
    genericParseRecord mods@Modifiers{..} = do
        let m :: M1 i c h a
            m = undefined

        let name = constructorNameModifier (conName m)

        let info = Options.info (Options.helper <*> (genericParseRecord mods)) mempty

        let subparserFields =
                   Options.command name info
                <> Options.metavar name

        let parser = Options.subparser subparserFields

        fmap L1 (genericParseRecord mods) <|> fmap (R1 . M1) parser

-- See: [NOTE - Sums]
instance (Constructor c1, Constructor c2, GenericParseRecord f1, GenericParseRecord f2) => GenericParseRecord (M1 C c1 f1 :+: M1 C c2 f2) where
    genericParseRecord mods@Modifiers{..} = do
        let m1 :: M1 i c1 f a
            m1 = undefined
        let m2 :: M1 i c2 g a
            m2 = undefined

        let name1 = constructorNameModifier (conName m1)
        let name2 = constructorNameModifier (conName m2)

        let info1 = Options.info (Options.helper <*> (genericParseRecord mods)) mempty
        let info2 = Options.info (Options.helper <*> (genericParseRecord mods)) mempty

        let subparserFields1 =
                   Options.command name1 info1
                <> Options.metavar name1
        let subparserFields2 =
                   Options.command name2 info2
                <> Options.metavar name2

        let parser1 = Options.subparser subparserFields1
        let parser2 = Options.subparser subparserFields2

        fmap (L1 . M1) parser1 <|> fmap (R1 . M1) parser2

instance (GenericParseRecord f, GenericParseRecord g) => GenericParseRecord (f :*: g) where
    genericParseRecord mods = liftA2 (:*:) (genericParseRecord mods) (genericParseRecord mods)

instance GenericParseRecord V1 where
    genericParseRecord _ = empty

instance (Selector s, ParseFields a) => GenericParseRecord (M1 S s (K1 i a)) where
    genericParseRecord Modifiers{..} = do
        let m :: M1 i s f a
            m = undefined

        let label = case selName m of
                ""   -> Nothing
                name -> Just (Data.Text.pack (fieldNameModifier name))
        let shortName = shortNameModifier (selName m)
        fmap (M1 . K1) (parseFields Nothing label shortName Nothing)

{- [NOTE - Sums]

   You might wonder why the `GenericParseRecord` instances for `(:+:)` are so
   complicated.  A much simpler approach would be something like this:

> instance (GenericParseRecord f, GenericParseRecord g) => GenericParseRecord (f :+: g) where
>     genericParseRecord = fmap L1 genericParseRecord <|> fmap R1 genericParseRecord
> 
> instance (Constructor c, GenericParseRecord f) => GenericParseRecord (M1 C c f) where
>     genericParseRecord = do
>         let m :: M1 i c f a
>             m = undefined
> 
>         let name = map toLower (conName m)
> 
>         let info = Options.info genericParseRecord mempty
> 
>         let subparserFields =
>                    Options.command n info
>                 <> Options.metavar n
> 
>         fmap M1 (Options.subparser subparserFields)

    The reason for the extra complication is so that datatypes with just one
    constructor don't have subcommands.  That way, if a user defines a data
    type like:

> data Example = Example { foo :: Double } deriving (Generic)
>
> instance ParseRecord Example

    .. then the command line will only read in the @--foo@ flag and won't
    expect a gratuitous @example@ subcommand:

> ./example --foo 2

    However, if a user defines a data type with two constructors then the
    subcommand support will kick in.

    Some other alternatives that I considered and rejected:

    * Alternative #1: Constructors prefixed with something like @Command_@ are
      turned into sub-commands named after the constructor with the prefix
      stripped.  If the prefix is not present then they don't get a subcommand.

        I rejected this approach for several reasons:

        * It's ugly
        * It's error-prone (consider the case: @data T = C1 Int | C2 Int@, which
          would never successfully parse @C2@).  Subcommands should be mandatory
          for types with multiple constructors
        * It doesn't work "out-of-the-box" for most types in the Haskell
          ecosystem which were not written with this library in mind

    * Alternative #2: Any constructor named some reserved name (like @Only@)
      would not generate a sub-command.

        I rejected this approach for a couple of reasons:

        * Too surprising.  The user would never know or guess about this
          behavior without reading the documentation.
        * Doesn't work "out-of-the-box" for single-constructor types in the
          Haskell ecosystem (like `(a, b)`, for example)
-}

instance GenericParseRecord f => GenericParseRecord (M1 D c f) where
    genericParseRecord mods = fmap M1 (Options.helper <*> genericParseRecord mods)

{-| Use `parseRecordWithModifiers` when you want to tweak the behavior of a
    derived `ParseRecord` implementation, like this:

    > myModifiers :: Modifiers
    > myModifiers = defaultModifiers { constructorNameModifier = id }
    >
    > instance ParseRecord MyType where
    >     parseRecord = parseRecordWithModifiers myModifiers

    This will still require that you derive `Generic` for your type to automate
    most of the implementation, but the `Modifiers` that you pass will change
    how the implementation generates the command line interface
-}
parseRecordWithModifiers
    :: (Generic a, GenericParseRecord (Rep a)) => Modifiers -> Parser a
parseRecordWithModifiers mods = fmap GHC.Generics.to (genericParseRecord mods)

-- | Marshal any value that implements `ParseRecord` from the command line
--
-- If you need to modify the top-level 'ParserInfo' or 'ParserPrefs'
-- use the 'getRecordWith' function.
getRecord
    :: (MonadIO io, ParseRecord a)
    => Text
    -- ^ Program description
    -> io a
getRecord desc = getRecordWith header mempty
  where
    header = Options.header (Data.Text.unpack desc)

-- | Marshal any value that implements `ParseRecord` from the command line
--
-- This is the lower-level sibling of 'getRecord and lets you modify
-- the 'ParserInfo' and 'ParserPrefs' records.
getRecordWith
    :: (MonadIO io, ParseRecord a)
    => Options.InfoMod a
    -- ^ 'ParserInfo' modifiers
    -> Options.PrefsMod
    -- ^ 'ParserPrefs' modifiers
    -> io a
getRecordWith infoMods prefsMods = liftIO (Options.customExecParser prefs info)
  where
    prefs  = Options.prefs (defaultParserPrefs <> prefsMods)
    info   = Options.info parseRecord infoMods

-- | Marshal any value that implements `ParseRecord` from the commmand line
-- alongside an io action that prints the help message.
getWithHelp
    :: (MonadIO io, ParseRecord a)
    => Text
    -- ^ Program description
    -> io (a, io ())
    -- ^ (options, io action to print help message)
getWithHelp desc = getWithHelpWith desc mempty

-- | Marshal any value that implements `ParseRecord` from the commmand line
-- alongside an io action that prints the help message.
getWithHelpWith
    :: (MonadIO io, ParseRecord a)
    => Text
    -- ^ Program description
    -> Options.PrefsMod
    -- ^ 'ParserPrefs' modifiers
    -> io (a, io ())
    -- ^ (options, io action to print help message)
getWithHelpWith desc prefsMods  = do
  a <- getRecordWith header prefsMods
  return (a, help)
  where
    header = Options.header (Data.Text.unpack desc)
    info   = Options.info parseRecord header
    help   = liftIO (showHelpText (Options.prefs defaultParserPrefs) info)

{-| Pure version of `getRecord`

If you need to modify the parser's 'ParserInfo' or 'ParserPrefs', use
`getRecordPureWith`.

>>> :set -XOverloadedStrings
>>> getRecordPure ["1"] :: Maybe Int
Just 1
>>> getRecordPure ["1", "2"] :: Maybe [Int]
Just [1,2]
>>> getRecordPure ["Foo"] :: Maybe Int
Nothing
-}
getRecordPure
    :: ParseRecord a
    => [Text]
    -- ^ Command-line arguments
    -> Maybe a
getRecordPure args = getRecordPureWith args mempty mempty

{-| Pure version of `getRecordWith`

Like `getRecordWith`, this is a sibling of 'getRecordPure and
exposes the monoidal modifier structures for 'ParserInfo' and
'ParserPrefs' to you.

>>> :set -XOverloadedStrings
>>> getRecordPureWith ["1"] mempty mempty :: Maybe Int
Just 1
>>> getRecordPureWith ["1", "2"] mempty mempty :: Maybe [Int]
Just [1,2]
>>> getRecordPureWith ["Foo"] mempty mempty :: Maybe Int
Nothing
-}
getRecordPureWith
    :: ParseRecord a
    => [Text]
    -- ^ Command-line arguments
    -> Options.InfoMod a
    -- ^ 'ParserInfo' modifiers
    -> Options.PrefsMod
    -- ^ 'ParserPrefs' modifiers
    -> Maybe a
getRecordPureWith args infoMod prefsMod = do
    let header = Options.header ""
    let info   = Options.info parseRecord (header <> infoMod)
    let prefs  = Options.prefs (defaultParserPrefs <> prefsMod)
    let args'  = map Data.Text.unpack args
    Options.getParseResult (Options.execParserPure prefs info args')

-- | @optparse-generic@'s flavor of options.
defaultParserPrefs :: Options.PrefsMod
defaultParserPrefs = Options.multiSuffix "..."

-- | A type family to extract fields wrapped using '(<?>)'
type family (:::) wrap wrapped
type instance Wrapped ::: wrapped = wrapped
type instance Unwrapped ::: wrapped = Unwrap wrapped

type family Unwrap ty where
  Unwrap (ty <?> helper) = Unwrap ty
  Unwrap (ty <!> defVal) = Unwrap ty
  Unwrap (ty <#> shrtNm) = Unwrap ty
  Unwrap ty = ty

infixr 0 :::

-- | Flag to keep fields wrapped
data Wrapped

-- | Flag to unwrap fields annotated using '(<?>)'
data Unwrapped

-- | Constraint for types whose fields can be unwrapped
type Unwrappable f = (Generic (f Wrapped), Generic (f Unwrapped), GenericUnwrappable (Rep (f Wrapped)) (Rep (f Unwrapped)))

class GenericUnwrappable f f' where
  genericUnwrap :: f p -> f' p

instance GenericUnwrappable U1 U1 where
  genericUnwrap = id

instance GenericUnwrappable f f' => GenericUnwrappable (M1 i c f) (M1 i c f') where
  genericUnwrap = M1 . genericUnwrap . unM1

instance (GenericUnwrappable f f', GenericUnwrappable g g') => GenericUnwrappable (f :+: g) (f' :+: g') where
  genericUnwrap (L1 f) = L1 (genericUnwrap f)
  genericUnwrap (R1 g) = R1 (genericUnwrap g)

instance (GenericUnwrappable f f', GenericUnwrappable g g') => GenericUnwrappable (f :*: g) (f' :*: g') where
  genericUnwrap (f :*: g) = genericUnwrap f :*: genericUnwrap g

instance GenericUnwrappable (K1 i c) (K1 i c) where
  genericUnwrap = id

instance GenericUnwrappable (K1 i field) (K1 i c)
  => GenericUnwrappable (K1 i (field <?> helper)) (K1 i c) where
    genericUnwrap (K1 c) = (genericUnwrap :: K1 i field p -> K1 i c p) (K1 (unHelpful c))

instance GenericUnwrappable (K1 i field) (K1 i c)
  => GenericUnwrappable (K1 i (field <!> defVal)) (K1 i c) where
    genericUnwrap (K1 c) = (genericUnwrap :: K1 i field p -> K1 i c p) (K1 (unDefValue c))

instance GenericUnwrappable (K1 i field) (K1 i c)
  => GenericUnwrappable (K1 i (field <#> defVal)) (K1 i c) where
    genericUnwrap (K1 c) = (genericUnwrap :: K1 i field p -> K1 i c p) (K1 (unShortName c))

-- | Unwrap the fields of a constructor
unwrap :: forall f . Unwrappable f => f Wrapped -> f Unwrapped
unwrap = to . genericUnwrap . from

-- | Marshal any value that implements 'ParseRecord' from the command line
-- and unwrap its fields
unwrapRecord
    :: (Functor io, MonadIO io, ParseRecord (f Wrapped), Unwrappable f)
    => Text
    -> io (f Unwrapped)
unwrapRecord = fmap unwrap . getRecord

-- | Pure version of `unwrapRecord`
unwrapRecordPure
    :: (ParseRecord (f Wrapped), Unwrappable f)
    => [Text]
    -- ^ Command-line arguments
    -> Maybe (f Unwrapped)
unwrapRecordPure = fmap unwrap . getRecordPure

showHelpText :: Options.ParserPrefs -> Options.ParserInfo a -> IO ()
showHelpText pprefs pinfo =
  Options.handleParseResult . Options.Failure $
  Options.parserFailure pprefs pinfo (Options.ShowHelpText Nothing) mempty

-- | Marshal any value that implements 'ParseRecord' from the command line
-- and unwrap its fields alongside an io action to print the help message
unwrapWithHelp
    :: (MonadIO io, ParseRecord (f Wrapped), Unwrappable f)
    => Text
    -- ^ Program description
    -> io (f Unwrapped, io ())
    -- ^ (options, io action to print help message)
unwrapWithHelp desc = do
  (opts, help) <- getWithHelp desc
  return (unwrap opts, help)
