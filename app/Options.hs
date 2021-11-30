{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Options
Description: Command-line arguments and options.
License:     MIT
Maintainer:  @tylerjl

There's a lot of optparse-generic boilerplate to handle, so we stuff it all in
here.
-}
module Options where

import Options.Generic

newtype Year = ArgYear Int deriving (Generic, Read)
instance ParseRecord Year
instance ParseField Year
instance ParseFields Year

newtype Day = ArgDay Int deriving (Generic, Read)
instance ParseRecord Day
instance ParseField Day

instance ParseFields Day
newtype Path = ArgPath String deriving (Generic, Read)
instance ParseRecord Path
instance ParseField Path
instance ParseFields Path

data Options w =
  Options (w ::: Year <?> "Year")
          (w ::: Day  <?> "Day")
          (w ::: Path <?> "Input file path")
  deriving Generic
instance ParseRecord (Options Wrapped)
