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

type Year = Int
type Day  = Int
type Path = String

data Options w =
  Options (w ::: Year <?> "Year")
          (w ::: Day  <?> "Day")
          (w ::: Path <?> "Input file path")
  deriving Generic
instance ParseRecord (Options Wrapped)
