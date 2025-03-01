main :: IO ()
main = do
  print (parse "{\"foo\":[1, {\"bar\":2},true],\"baz\":null}")
  print (parse "{\"parent\":{\"child\":{\"name\":\"Martin\"},\"name\":\"John\"}}")

jsonWhitespace :: String
jsonWhitespace = " \t\b\n\r"

jsonSyntax :: String
jsonSyntax = ",:[]{}"

numberCharacters :: String
numberCharacters = ['0' .. '9'] ++ ['-', 'e', '.']

lexString :: String -> (String, String)
lexString ('"' : xs) =
  let (string, rest) = break (== '"') xs
   in (string, drop 1 rest)

lexNumber :: String -> (String, String)
lexNumber = break (`notElem` numberCharacters)

lex' :: String -> [String]
lex' [] = []
lex' ('"' : xs) =
  let (string, rest) = lexString ('"' : xs)
   in string : lex' rest
lex' (x : xs)
  | x `elem` numberCharacters =
      let (number, rest) = lexNumber (x : xs)
       in number : lex' rest
  | take 4 (x : xs) == "true" = "true" : lex' (drop 3 xs)
  | take 5 (x : xs) == "false" = "false" : lex' (drop 4 xs)
  | take 4 (x : xs) == "null" = "null" : lex' (drop 3 xs)
  | x `elem` jsonWhitespace = lex' xs
  | x `elem` jsonSyntax = [x] : lex' xs
  | otherwise = error ("Unexpected character: " ++ [x])

data JSONValue
  = JSONString String
  | JSONNumber Double
  | JSONBool Bool
  | JSONNull
  | JSONArray [JSONValue]
  | JSONObject [(String, JSONValue)]
  deriving (Show, Eq)

parseValue :: String -> JSONValue
parseValue "true" = JSONBool True
parseValue "false" = JSONBool False
parseValue "null" = JSONNull
parseValue x
  | all (`elem` numberCharacters) x = JSONNumber (read x)
  | otherwise = JSONString x

parseArray :: [String] -> JSONValue
parseArray ("[" : xs) = JSONArray (parseElements xs)
  where
    parseElements :: [String] -> [JSONValue]
    parseElements ("]" : xs) = []
    parseElements ("{" : xs) = parseObject ("{" : takeWhile (/= "}") xs) : parseElements (drop 1 (dropWhile (/= "}") xs))
    parseElements ("," : xs) = parseElements xs
    parseElements (x : xs) = parseValue x : parseElements xs
parseArray _ = error "Invalid JSON array"

parseObject :: [String] -> JSONValue
parseObject ("{" : xs) = JSONObject (parsePairs xs)
  where
    parsePairs :: [String] -> [(String, JSONValue)]
    parsePairs ("}" : xs) = []
    parsePairs (x : ":" : xs) =
      case parsePairValue xs of
        (value, "," : rest) -> (x, value) : parsePairs rest
        (value, rest) -> [(x, value)]

    parsePairValue :: [String] -> (JSONValue, [String])
    parsePairValue ("{" : xs) =
      let object = parseObject ("{" : xs)
       in (object, drop 1 (dropWhile (/= "}") xs))
    parsePairValue ("[" : xs) =
      let array = parseArray ("[" : xs)
       in (array, drop 1 (dropWhile (/= "]") xs))
    parsePairValue (x : xs) = (parseValue x, xs)
parseObject _ = error "Invalid JSON object"

parse :: String -> JSONValue
parse = parseObject . lex'