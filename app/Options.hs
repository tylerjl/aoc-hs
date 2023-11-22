{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

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

type Measure = Bool
type Year = Int
type Day  = Int
type Part = Char
type Path = String

newtype Flags = Flags { timed :: Bool } deriving Generic
instance ParseRecord Flags where

data Arguments =
  Arguments Year Day Part Path [String] deriving Generic
instance ParseRecord Arguments

data Options = Options Flags Arguments
instance ParseRecord Options where
  parseRecord = Options <$> parseRecord <*> parseRecord
