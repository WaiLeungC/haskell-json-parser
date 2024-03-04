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
    parseElements ("{" : xs) = [parseObject ("{" : xs)]
    parseElements ("," : xs) = parseElements xs
    parseElements (x : xs) = parseValue x : parseElements xs
parseArray _ = error "Invalid JSON array"

parseObject :: [String] -> JSONValue
parseObject ("{" : xs) = JSONObject (parsePairs xs)
  where
    parsePairs :: [String] -> [(String, JSONValue)]
    parsePairs ("}" : xs) = []
    parsePairs ("," : xs) = parsePairs xs
    parsePairs (x : ":" : y : xs) =
      let value = case y of
            "{" -> parseObject (y : xs)
            "[" -> parseArray (y : xs)
            _ -> parseValue y
       in (x, value) : parsePairs (tail xs)
    parsePairs _ = []
parseObject _ = error "Invalid JSON object"

parse :: String -> JSONValue
parse = parseObject . lex'

main :: IO ()
main = do
  print (lexString "\"foo\": []")
  print (lexNumber "123: []")

  let tokens = lex' "{\"foo\": [1, true, {\"bar\": 2}],\"baz\": null}"
  print tokens

  let jsonValue = JSONObject [("name", JSONString "John"), ("age", JSONNumber 20), ("isStudent", JSONBool True), ("hobbies", JSONArray [JSONString "Reading", JSONString "Coding"])]
  print jsonValue

  print (parseArray ["[", "1", ",", "true", ",", "example", "]"])
  print (parseObject ["{", "hobby", ":", "Reading", "}"])
  print (parseObject ["{", "name", ":", "John", ",", "age", ":", "20", "}"])
  print (parseObject ["{", "hobbies", ":", "[", "Reading", "]", "}"])
  print (parseObject ["{", "hobbies", ":", "[", "Reading", ",", "Coding", "]", "}"])
  print (parseObject ["{", "address", ":", "{", "street", ":", "Streetname", "}", "}"])
  print (parseObject ["{", "address", ":", "{", "street", ":", "Streetname", ",", "city", ":", "Cityname", "}", "}"])

  print (parse "{\"hobbies\":[\"Reading\",\"Coding\"]}")
  print (parse "{\"address\":{\"street\":\"Streetname\",\"city\":\"Cityname\"}}")

  print (parse "{\"foo\":[{\"bar\":2}]}")
  print (parse "{\"foo\": [1, true, {\"bar\": 2}],\"baz\": null}")