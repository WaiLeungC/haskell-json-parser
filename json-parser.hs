jsonWhitespace :: String
jsonWhitespace = " \t\b\n\r"

jsonSyntax :: String
jsonSyntax = ",:[]{}"

numberCharacters :: String
numberCharacters = [character | digit <- ['0' .. '9'], character <- [digit]] ++ ['-', 'e', '.']

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

main :: IO ()
main = do
  print (lexString "\"foo\": []")
  print (lexNumber "123: []")
  print (lex' "{\"foo\": [1, true, {\"bar\": 2}],\"baz\": null}")