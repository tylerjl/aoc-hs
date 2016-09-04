module Y2015.D08 (difference, encoded) where

body :: [a] -> [a]
body []  = []
body [x] = []
body xs  = tail $ init xs

hexChars :: String
hexChars = "0123456789abcdef"

escape :: String -> String
escape []                                   = []
escape [x]                                  = [x]
escape (v:w:x:y:zs) | [v,w] == "\\x" && hex = '.'  : escape zs
                    where hex = all (`elem` hexChars) [x,y]
escape (x:y:zs)     | [x,y] == "\\\""       = '"'  : escape zs
                    | [x,y] == "\\\\"       = '\\' : escape zs
                    | otherwise             = x    : escape (y:zs)

encode :: String -> String
encode = (++) "\"" . (:) '"' . encode'

encode' :: String -> String
encode' []                 = []
encode' (x:xs) | x == '"'  = '\\' : '"' : encode' xs
               | x == '\\' = "\\\\"    ++ encode' xs
               | otherwise = x          : encode' xs

squish :: String -> String
squish = filter (/= '\n')

solve :: (Int -> Int -> Int) -> (String -> String) -> String -> Int
solve g f s = g (original s) (new s)
    where original = length . squish
          new      = length . squish . unlines . map f . lines

difference :: String -> Int
difference = solve (-) (escape . body)

encoded :: String -> Int
encoded = solve (flip (-)) encode
