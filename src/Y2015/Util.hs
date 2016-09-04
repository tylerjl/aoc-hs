module Y2015.Util
    ( (<&&>)
    , regularParse
    , intParser
) where

import           Control.Monad (liftM2)
import qualified Text.Parsec as     P
import           Text.Parsec.Char   (digit)
import           Text.Parsec.String (Parser)

(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<&&>) = liftM2 (&&)

-- |Generic parsing wrapper
regularParse :: Parser a -> String -> Either P.ParseError a
regularParse p = P.parse p ""

intParser :: Parser Int
intParser = read <$> P.many1 digit
