module Parser () where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Utils
import Data.Maybe
import qualified Data.Bool as B
import qualified Data.Map as M
import System.IO

data LispVal = LSymbol String
             | LList [LispVal]
             | LInteger Integer
             | LFloat Double
             | LString String
             | LBool Bool
             | LPrimitive ([LispVal]->LispVal)
             | LLambda String Environment [LispVal] LispVal
             | LError String



type Environment = M.Map String LispVal


specialChar :: Parser Char
specialChar = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ LString x


parseSymbol :: Parser LispVal
parseSymbol = do first <- letter <|> specialChar
                 rest <- many (letter <|> specialChar <|> digit)
                 let symbol= first:rest
                 return $ case symbol of
                   "true" -> LBool True
                   "false" -> LBool False
                   _ -> LSymbol symbol


--parseInteger :: Parser LispVal

parseInteger :: Parser LispVal
--parseInteger = liftM (Integer . read) $ many1 digit

parseInteger = do lookAhead $ many1 digit >> notFollowedBy (oneOf ".")
                  ds <- many1 digit
                  return $ (LInteger . read) ds

--parseInteger = fmap (Integer . read) $ many1 digit


--parseNumber :: Parser LispVal
--parseNumber = lookAhead

parseFloat :: Parser LispVal
parseFloat = liftM (LFloat . read) $ do d1 <- (many digit)
                                        c <- (char '.')
                                        d2 <- (many digit)
                                        return $ d1++[c]++d2

parseList :: Parser LispVal
parseList = liftM LList $ sepBy parseExpr spaces

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ LList [LSymbol "quote",x]

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
-- TODO error handling
readExpr input = case parse parseExpr "lisp" input of
  Left err -> LString $ show err
  Right val -> val


showVal :: LispVal -> String
showVal (LString c) = "\"" ++ c ++ "\""
showVal (LSymbol name) = name
showVal (LInteger i) = show i
showVal (LFloat f) = show f
showVal (LBool True) = "true"
showVal (LBool False) = "false"
showVal (LList c) = "(" ++ unwordsList c ++ ")"
showVal (LError e) = "Error : " ++ e
showVal l@(LLambda name e args body) = "<lambda : " ++ name ++ "-> env: " ++ show e ++ " body:" ++ show body ++ ">"
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

type EnvVal = (Environment, LispVal)


eval :: Environment -> LispVal -> EnvVal
eval e v@(LString _) = (e,v)
eval e v@(LInteger _) = (e,v)
eval e v@(LBool _) = (e,v)
eval e v@(LFloat _) = (e,v)
eval e v@(LError _) = (e,v)
eval e (LList [LSymbol "quote", v]) = (e,v)
eval e v@(LList []) = (e,v)

eval e (LSymbol "!debug!") = (e, LString $ show e)

eval e (LList [LSymbol "def", LSymbol name, LList [LSymbol "fn", LList bindings, body@(LList _)]]) =
  let     newLambda = LLambda name e bindings body
          newEnv = M.insert name newLambda e
  in
  (newEnv, newLambda)



eval env (LList [LSymbol "def", LSymbol name, v]) = ((M.insert name (snd (eval env v)) env), LSymbol name)


eval e (LList [LSymbol "fn", LList bindings, body@(LList _)]) = (e, LLambda "" e bindings body)

eval e v@(LLambda _ _ _ _) = (e,v)

eval e (LList ( l@(LLambda name lenv bindings body) : args)) = applyLambda l $ map (snd . (eval lenv)) args

eval env (LList (LSymbol func : args)) = (env, apply env func $ map (snd . (eval env)) args)

eval env (LSymbol name) = (env, lookupSymbol env name [])


--eval e (LList (LSymbol "fn": others)

applyLambda :: LispVal -> [LispVal] -> EnvVal
applyLambda (LLambda name lenv bindings body) args
  | length args == length bindings = eval (M.union lenv (M.fromList (zip (map show bindings) args))) body
  | otherwise = (lenv, LError "arity error")

apply :: Environment -> String -> [LispVal] -> LispVal
apply env func args =
  case M.lookup func $ M.union listPrimitives env of
    Just (LPrimitive f) -> (f args)
    Just l@(LLambda _ _ _ _) -> snd $ applyLambda l args
    Nothing -> inferTypeAndApply func args

inferTypeAndApply :: String -> [LispVal] -> LispVal
inferTypeAndApply func args
  | isIntList args = lookupSymbol intPrimitives func args
  | isFloatList args = lookupSymbol floatPrimitives func args
  | isBoolList args = lookupSymbol boolPrimitives func args
  | otherwise = LError "symbol not defined"


lookupSymbol :: Environment -> String -> [LispVal] -> LispVal
lookupSymbol env name args =
  case M.lookup name env of
    Just (LPrimitive v) -> (v args)
    Just e -> e
    Nothing -> (LError $ "symbol '" ++ name ++ "' not defined")


intPrimitives :: M.Map String LispVal
intPrimitives = M.fromList [("+", LPrimitive $ intBinaryOp (+)),
                            ("-", LPrimitive $ intBinaryOp (-)),
                            ("*", LPrimitive $ intBinaryOp (*)),
                            ("/", LPrimitive $ intBinaryOp (quot)),
                            ("mod", LPrimitive $ intBinaryOp (rem)),
                            ("pow", LPrimitive $ intBinaryOp (^))]


intBinaryOp :: (Integer -> Integer -> Integer)->([LispVal]->LispVal)
intBinaryOp f = (\ args ->
                  case args of
                    (LInteger x):(LInteger y):[] -> LInteger (f x y)
                    _ -> LError "arity error")


floatPrimitives :: Environment
floatPrimitives = M.fromList [("+", LPrimitive $ floatBinaryOp (+)),
                              ("-", LPrimitive $ floatBinaryOp (-)),
                              ("*", LPrimitive $ floatBinaryOp (*)),
                              ("/", LPrimitive $ floatBinaryOp (/)),
                              ("pow", LPrimitive $ floatBinaryOp (**))]

floatBinaryOp :: (Double -> Double -> Double)->([LispVal]->LispVal)
floatBinaryOp f = (\ args ->
                    case args of
                      (LFloat x):(LFloat y):[] -> LFloat (f x y)
                      _ -> LError "arity error")


boolPrimitives :: Environment
boolPrimitives = M.fromList [("and", LPrimitive $ boolBinaryOp (B.&&)),
                             ("or", LPrimitive $ boolBinaryOp (B.||)),
                             --("xor", LPrimitive $ boolBinaryOp (B.xor)),
                             ("not", LPrimitive $ (\args ->
                                                    case args of
                                                      (LBool x):[] -> LBool $ B.not x
                                                      _ -> LError "arity error"))]


boolBinaryOp :: (Bool -> Bool -> Bool) -> ([LispVal]->LispVal)
boolBinaryOp f = (\args ->
                   case args of
                     (LBool x):(LBool y):[] -> LBool (f x y)
                     _ -> LError "arity error")

toLList :: [LispVal] -> LispVal
toLList = foldr (\ l acc -> LList [LSymbol "cons", l, acc]) (LList [])



cons :: [LispVal] -> LispVal
cons args =
  case args of
    x:y:[] ->   LList [LSymbol "cons",x,y]
    _ -> LError "arity error"

first :: [LispVal] -> LispVal
first ((LList [LSymbol "cons",f,r]):[]) = f
first  _ = LError "illegal arguments"

rest :: [LispVal] -> LispVal
rest ((LList [LSymbol "cons",f,r]):[]) = r
rest  _ = LError "illegal arguments"

listPrimitives :: Environment
listPrimitives = M.fromList [("list", LPrimitive (toLList)),
                             ("cons", LPrimitive (cons)),
                             ("first", LPrimitive (first)),
                             ("rest", LPrimitive (rest))
                            ]


isInt :: LispVal ->Bool
isInt (LInteger _) = True
isInt _ = False


isIntList :: [LispVal] -> Bool
isIntList = foldl (\ acc x -> and [acc, (isInt x)]) True


isFloat :: LispVal -> Bool
isFloat (LFloat _) = True
isFloat _ = False

isFloatList = foldl (\acc x -> and [acc, (isFloat x)]) True

isBoolList = foldl (\acc x -> and [acc, (isBool x)]) True
  where
    isBool (LBool _) = True
    isBool _ = False

readAndEval :: Environment -> String -> EnvVal
readAndEval env input = eval env $ readExpr input

printEval :: EnvVal -> IO ()
printEval p = putStrLn (show (snd p))

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

repl :: Environment -> IO ()
repl env = (readPrompt "Hisp>>> ") >>=
           (\ inp ->
             if inp == "quit" then
               return ()
             else
               let pair = readAndEval env inp
               in
                printEval pair >>=
                (\_ -> repl (fst pair)))


main = repl M.empty
