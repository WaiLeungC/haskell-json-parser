jsonWhitespace :: String
jsonWhitespace = " \t\b\n\r"

jsonSyntax :: String
jsonSyntax = ",:[]{}"

lexString :: String -> Maybe (String, String)
lexString ('"' : xs) = Just (takeWhile (/= '"') xs, '"' : xs)
lexString _ = Nothing

lex' :: String -> [String]
lex' [] = []
lex' ('"' : xs) = case lexString ('"' : xs) of
  Nothing -> error "Invalid string"
lex' (x : xs)
  | x `elem` jsonWhitespace = lex' xs
  | x `elem` jsonSyntax = [x] : lex' xs
  | otherwise = error ("Unexpected character: " ++ [x])

main :: IO ()
main = do
  print (lexString "\"foo\": []")

-- print (lex' "{\"foo\": [ , , {\"bar\": }]}")