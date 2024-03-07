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
      case parseRest xs of
        (value, "," : rest) -> (x, value) : parsePairs rest
        (value, rest) -> [(x, value)]

    parseRest :: [String] -> (JSONValue, [String])
    parseRest (x : xs)
      | x == "{" = let object = parseObject (x : xs) in (object, drop 1 (dropWhile (/= "}") xs))
      | x == "[" = let array = parseArray (x : xs) in (array, drop 1 (dropWhile (/= "]") xs))
      | otherwise = (parseValue x, xs)
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
  print (parseObject ["{", "name", ":", "John", "}"])
  print (parseObject ["{", "name", ":", "John", ",", "age", ":", "20", ",", "isStudent", ":", "true", "}"])
  print (parseObject ["{", "hobbies", ":", "[", "Reading", "]", "}"])
  print (parseObject ["{", "hobbies", ":", "[", "Reading", ",", "Coding", "]", "}"])
  print (parse "{\"hobbies\":[\"Reading\",\"Coding\"]}")
  print (parseObject ["{", "address", ":", "{", "street", ":", "Streetname", "}", "}"])
  print (parseObject ["{", "address", ":", "{", "street", ":", "Streetname", ",", "city", ":", "Cityname", "}", "}"])
  print (parse "{\"address\":{\"street\":\"Streetname\",\"city\":\"Cityname\"}}")

  print (JSONObject [("foo", JSONArray [JSONObject [("bar", JSONNumber 2.0)]])])
  print (parseObject ["{", "foo", ":", "[", "{", "bar", ":", "2", "}", "]", "}"])
  print (parse "{\"foo\":[{\"bar\":2}]}")

  print "----------------------------------------------------"

  -- [[]] ...
  print ((reverse . dropWhile (/= "]") . reverse) ["[", "bar", ",", "2", "]", ",", "baz", ":", "null", "}"])
  -- [...], key : value xs
  print (drop 1 (reverse (takeWhile (/= "]") (reverse ["[", "bar", ",", "2", "]", ",", "baz", ":", "null", "}"]))))

  print (parse "{\"foo\":[\"bar\", 2],\"baz\":null}")

  print (JSONObject [("foo", JSONArray [JSONObject [("bar", JSONNumber 2)]]), ("baz", JSONNull)])
  print (parseObject ["{", "foo", ":", "[", "{", "bar", ":", "2", "}", "]", ",", "baz", ":", "null", "}"])
  print (parse "{\"foo\":[{\"bar\":2}],\"baz\":null}")

  print (JSONObject [("foo", JSONArray [JSONNumber 1, JSONBool True, JSONObject [("bar", JSONNumber 2)]]), ("baz", JSONNull), ("foo2", JSONNumber 3)])
  print (parseObject ["{", "foo", ":", "[", "1", ",", "true", ",", "{", "bar", ":", "2", "}", "]", ",", "baz", ":", "null", ",", "foo2", ":", "3", "}"])
  print (parse "{\"foo\":[1,true,{\"bar\":2,\"bar2\":3},4,5],\"baz\":null,\"foo2\":3}")

  print (parse "{\"foo\":true,\"baz\":null, \"foo2\":3}")
  print (parse "{\"foo\":[1,true],\"baz\":null, \"foo2\":3}")
  print (parse "{\"foo\":[{\"foo3\":1},true],\"baz\":null, \"foo2\":3}")

  print (parseObject ["{", "parent", ":", "{", "child", ":", "{", "name", ":", "Child", "}", ",", "name", ":", "Parent", "}", "}"])