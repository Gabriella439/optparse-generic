{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import Options.Generic

data Options w = Options
  { a :: w ::: Int <?> "Int option -- must be divisible by 10"
  , b :: w ::: Bool <?> "Bool option"
  , c :: w ::: String <?> "String option"
  } deriving Generic

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  (opts, help) <- unwrapWithHelp "unwrap-example"
  if (a opts) `rem` 10 == 0
  then print (opts :: Options Unwrapped)
  else help
