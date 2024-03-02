jsonWhitespace :: String
jsonWhitespace = " \t\b\n\r"

jsonSyntax :: String
jsonSyntax = ",:[]{}"

lexString :: String -> (String, String)
lexString ('"' : xs) =
  let (string, rest) = span (/= '"') xs
   in (string, drop 1 rest)

lex' :: String -> [String]
lex' [] = []
lex' ('"' : xs) =
  let (string, rest) = lexString ('"' : xs)
   in string : lex' rest
lex' (x : xs)
  | x `elem` jsonWhitespace = lex' xs
  | x `elem` jsonSyntax = [x] : lex' xs
  | otherwise = error ("Unexpected character: " ++ [x])

main :: IO ()
main = do
  print (lexString "\"foo\": []")
  print (lex' "{\"foo\": [ , , {\"bar\": }]}")