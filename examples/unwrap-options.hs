{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import Options.Generic

data Options w = Options
  { a :: w ::: Int <?> "Int option"
  , b :: w ::: Bool <?> "Bool option"
  , c :: w ::: String <?> "String option"
  } deriving Generic

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  opts <- unwrapRecord "unwrap-example"
  print (opts :: Options Unwrapped)
