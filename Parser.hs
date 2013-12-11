module Parser () where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


data LispVal = Symbol String
             | List [LispVal]
             | Integer Integer
             | Float Double
             | String String
             | Bool Bool

specialChar :: Parser Char
specialChar = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


parseSymbol :: Parser LispVal
parseSymbol = do first <- letter <|> specialChar
                 rest <- many (letter <|> specialChar <|> digit)
                 let symbol= first:rest
                 return $ case symbol of
                   "true" -> Bool True
                   "false" -> Bool False
                   _ -> Symbol symbol


--parseInteger :: Parser LispVal

parseInteger :: Parser LispVal
--parseInteger = liftM (Integer . read) $ many1 digit

parseInteger = do lookAhead $ many1 digit >> notFollowedBy (oneOf ".")
                  ds <- many1 digit
                  return $ (Integer . read) ds

--parseInteger = fmap (Integer . read) $ many1 digit


--parseNumber :: Parser LispVal
--parseNumber = lookAhead

parseFloat :: Parser LispVal
parseFloat = liftM (Float . read) $ do d1 <- (many digit)
                                       c <- (char '.')
                                       d2 <- (many digit)
                                       return $ d1++[c]++d2

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Symbol "quote",x]

parseExpr :: Parser LispVal
parseExpr = parseSymbol <|>
            parseString <|>
            parseQuoted <|>
            try (parseInteger) <|>
            parseFloat <|>
            do char '('
               x <- parseList
               char ')'
               return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ show err
  Right val -> val


showVal :: LispVal -> String
showVal (String c) = "\"" ++ c ++ "\""
showVal (Symbol name) = name
showVal (Integer i) = show i
showVal (Float f) = show f
showVal (Bool True) = "true"
showVal (Bool False) = "false"
showVal (List c) = "(" ++ unwordsList c ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval v@(String _) = v
eval v@(Integer _) = v
eval v@(Bool _) = v
eval v@(Float _) = v
eval (List [Symbol "quote", v]) = v

repl :: String -> String
repl =  show . eval . readExpr
