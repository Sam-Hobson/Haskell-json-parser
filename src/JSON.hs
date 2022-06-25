-- | Implement a JSON parser.
module JSON where

import Data.Char

import Instances
import Parser
import Specials

-- | Associative container type.
type Assoc = [(String, JsonValue)]

-- | JSON value representation.
data JsonValue =
     JsonString String
   | JsonRational !Rational
   | JsonObject Assoc
   | JsonArray [JsonValue]
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)

-- >>> parse (between (is '[') (is ']') character) "[a]"
-- Result >< 'a'
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "abc]")
-- True
between :: Parser o -> Parser c -> Parser a -> Parser a
between p1 p2 p3 = do
  _ <- p1
  x <- p3
  _ <- p2
  return x

-- >>> parse (betweenCharTok '[' ']' character) "[a]"
-- Result >< 'a'
--
-- >>> parse (betweenCharTok '[' ']' (charTok 'a')) "[ a ] "
-- Result >< 'a'
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
-- True
betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok c1 c2 p = charTok c1 >> (p <* charTok c2)

-- >>> parse hex "0010"
-- Result >< '\DLE'
--
-- >>> parse hex "0a1f"
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hex "001")
-- True
--
-- >>> isErrorResult (parse hex "0axf")
-- True
hex :: Parser Char
hex = thisMany 4 (satisfy isHexDigit) >>= (\d -> case readHex d of
  Just (x,_) -> pure $ chr x
  Nothing -> failed $ UnexpectedString d)

-- >>> parse hexu "u0010"
-- Result >< '\DLE'
--
-- >>> parse hexu "u0a1f"
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hexu "0010")
-- True
--
-- >>> isErrorResult (parse hexu "u001")
-- True
--
-- >>> isErrorResult (parse hexu "u0axf")
-- True
hexu :: Parser Char
hexu = is 'u' >> hex

-- >>> parse specialChar "b"
-- Result >< '\b'
--
-- >>> parse specialChar "\""
-- Result >< '"'
--
-- >>> isErrorResult (parse specialChar "a")
-- True
specialChar :: Parser Char
specialChar = character >>= (\d -> case fromSpecialCharacter <$> toSpecialCharacter d of
  Just a -> pure a
  Nothing -> failed $ UnexpectedString [d]
  )

-- >>> parse jsonSpecial "\\u0af3"
-- Result >< '\2803'
--
-- >>> parse jsonSpecial "\\b"
-- Result >< '\b'
--
-- >>> isErrorResult (parse jsonSpecial "\\a")
-- True
jsonSpecial :: Parser Char
jsonSpecial = specialChar >> (hexu ||| specialChar)

-- /Hint/: Use 'between', 'is', 'charTok', 'noneof', 'jsonSpecial'.
--
-- /Hint/: The inner parser needs to /fail/ in order to trigger the second
-- delimiter.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"abc\"   def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"\\babc\"def"
-- Result >def< "\babc"
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Result >def< "\171c"
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Result >def< "\255abc"
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Result >def< "\250abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString :: Parser String
jsonString = between (is '\"') (charTok '\"') (list $ jsonSpecial ||| noneof "\\\"")

-- | Parse a JSON rational.
--
-- /Hint/: Use 'readFloats', 'tok'.
--
-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber :: Parser Rational
jsonNumber = tok (P $ \s -> case readFloats s of
  Nothing -> Error $ UnexpectedString s
  Just (x, r) -> Result r x
  )

-- | Parse a JSON true literal.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue :: Parser String
jsonTrue = stringTok "true"

-- | Parse a JSON false literal.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse :: Parser String
jsonFalse = stringTok "false"

-- | Parse a JSON null literal.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull :: Parser String
jsonNull = stringTok "null"

-- /Hint/: Use 'betweenCharTok', 'sepby' and 'commaTok'.
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[a]"
-- Result >< "a"
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[]"
-- Result >< ""
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[a,b,c]"
-- Result >< "abc"
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
-- True
betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma c1 c2 p = betweenCharTok c1 c2 (sepby p commaTok)

-- | Parse a JSON array.
--
-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]
jsonArray :: Parser [JsonValue]
jsonArray = betweenSepbyComma '[' ']' jsonValue

-- | Parse a JSON object.
--
-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]
jsonObject :: Parser Assoc
jsonObject = betweenSepbyComma '{' '}' (((,) <$> (jsonString <* charTok ':')) <*> jsonValue)

-- | Parse a JSON value.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]
jsonValue :: Parser JsonValue
jsonValue = spaces >> ((JsonTrue <$ jsonTrue) ||| (JsonFalse <$ jsonFalse) ||| (JsonNull <$ jsonNull) |||
                       (JsonArray <$> jsonArray) ||| (JsonRational <$> jsonNumber) ||| (JsonString <$> jsonString) |||
                       (JsonObject <$> jsonObject))

-- | Read a file into a JSON value.
readJsonValue :: FilePath -> IO (ParseResult JsonValue)
readJsonValue f = parse jsonValue <$> readFile f
