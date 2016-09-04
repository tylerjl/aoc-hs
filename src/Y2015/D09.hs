{-# LANGUAGE FlexibleContexts #-}

module Y2015.D09 (longestRoute, routeParser, shortestRoute) where

import Y2015.Util (regularParse, intParser)

import           Control.Applicative ((<*>))
import           Data.List           (concatMap, permutations)
import           Data.Map            ((!), Map, fromListWith, keys, singleton, union)
import qualified Data.Map as         Map
import           Data.Maybe          (mapMaybe)
import           Safe                (maximumMay, minimumMay)
import           Text.Parsec         (many1, optional, skipMany1, string)
import           Text.Parsec.Char    (char, digit, endOfLine, letter, space)
import           Text.Parsec.String  (Parser)

type City     = String
type Distance = Int
data Route    = Route City City Distance
              deriving (Show, Eq)

routeParser :: Parser [Route]
routeParser = many1 (parseRoute <* optional endOfLine)

parseRoute :: Parser Route
parseRoute = Route <$> many1 letter <* pSep "to"
                   <*> many1 letter <* pSep "="
                   <*> intParser
    where pSep s = many1 space *> string s *> skipMany1 space

parseRoutes :: String -> Maybe [Route]
parseRoutes s = case regularParse routeParser s of
                     Right routes -> Just routes
                     Left _       -> Nothing

-- Each key represents the start, with possible destination values,
-- which are keys to the distance to that destination.
-- We flip destinations to fully express all available routes.
drawMap :: [Route] -> Map City (Map City Distance)
drawMap = fromListWith union . map toMap . concatMap backTrack
    where toMap (Route x y d) = (x, singleton y d)
          backTrack (Route x y d) = [ Route x y d
                                    , Route y x d ]

shortestRoute :: [Route] -> Maybe Distance
shortestRoute = minimumMay . chart

longestRoute :: [Route] -> Maybe Distance
longestRoute = maximumMay . chart

-- Create the map structure, gather a permutation of all cities,
-- and build the distance for each permutation. There's a chance
-- some cities will be unroutable (a partition on the routes), so
-- there's a possibility we may return an empty list (this is an internal
-- function though, so we account for that in the above wrapper
-- functions.)
chart :: [Route] -> [Distance]
chart routes = mapMaybe (plot . (zip <*> tail)) . permutations . keys $ worldMap
    where worldMap      = drawMap routes
          plot          = fmap sum . mapM travel
          travel (a, b) | a `Map.member` worldMap = Map.lookup b (worldMap ! a)
                        | otherwise               = Nothing
