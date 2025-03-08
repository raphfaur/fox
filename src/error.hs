module Parser where
    import Control.Monad
    import qualified Control.Monad as Control
    import qualified Control.Applicative as Control.Monad
    import Control.Applicative (many, (<|>), some)
    import Debug.Trace
    import Grammar ( Expr (Number, Boolean, Sum, Minus, If, Var, Let, Compare, Assign, Block))
    import Data.Char (ord)

    -- trace a b = b
    -- Metadata
    data Meta = Data {ins :: String, c :: Int, l :: Int} | Bottom

    incC :: Meta -> Meta
    incC (Data s c l) = Data s (c+1) l

    incL :: Meta -> Meta
    incL (Data s c l) = Data s 0 (l + 1)



    instance Show Meta where
        show m = " at location : column " ++ show (c m) ++ " line " ++ show (l m)

    def = Data "" 0

    sw:: String -> Meta -> Meta
    sw s m = Data s (c m) (l m)

    wrap:: String -> Meta
    wrap s = Data s 0 0

    ----------------
    -- ParseError --
    ----------------

    data ParseError a = ParseError {errorS :: String} | Ok { okM :: a}

    instance (Show a) => Show (ParseError a) where
        show (ParseError e) = e
        show (Ok a) = show a

    instance Control.Monad.Functor ParseError where
        fmap f p = case p of
            ParseError s -> ParseError s
            Ok a -> Ok (f a)

    instance Control.Monad.Applicative ParseError where
      pure :: a -> ParseError a
      pure a = Ok a

      (<*>) :: ParseError (a -> b) -> ParseError a -> ParseError b
      f <*> a = case (f, a) of
        (ParseError e, _) -> ParseError e
        (Ok a, ParseError e) -> ParseError e
        (Ok f , Ok a) -> Ok (f a)

    instance Control.Monad.Monad ParseError where
      (>>=) result func = case result of
        Ok a-> func a
        ParseError s -> ParseError s

    ------------
    -- Parser --
    ------------

    newtype Parser a = Parser {runParser :: Meta -> ParseError (a, Meta)}

    instance Control.Monad.Functor Parser where
        fmap f p = Parser $ \m -> do
            (a,m') <- runParser p m
            return (f a, m')

    instance Control.Monad.Applicative Parser where
        pure x = Parser $ \m -> Ok (x , m)

        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        Parser f <*> Parser a = Parser $ \m -> do
            (func, m') <- f m
            (d', m'') <- a m'
            return (func d', m'')

    instance Control.Monad.Monad Parser where
        (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        Parser parser >>= func = Parser $ \m -> do
            (d, m') <- parser m
            runParser (func d) m'

    instance Control.Monad.Alternative Parser where
        empty = Parser $ \s -> ParseError ""
        Parser a <|> Parser b = Parser $ \s ->
            case a s of
                Ok (o, s') -> Ok (o, s')
                ParseError _ -> b s

    -----------
    -- UTILS --
    -----------

    char':: Char -> Parser Char
    char' c = Parser charP where
        charP m = case ins m of
            [] -> ParseError ""
            (x:xs) -> fn x where
                fn ch | c == x = Ok (c, sw xs m)
                      | otherwise = ParseError ""


    metaUpdater :: Char -> Parser Char
    metaUpdater c = Parser charP where
        charP m | c == '\n' = Ok (c, incL m)
                | otherwise = Ok (c, incC m)


    char :: Char -> Parser Char
    char c = char' c >>= metaUpdater

    string :: String -> Parser String
    string = mapM char

    handleError :: Parser a -> String -> Parser a
    handleError p s = Parser $ \m ->
        let a = runParser p m in
        case a of
            Ok (a, m) -> Ok (a, m)
            ParseError e -> ParseError (s ++ show m)

    -- Keyword --
    keyword:: String -> Parser String
    keyword s = handleError (string s) ("Expected keyword " ++ s)

    -- Spaces --
    ss :: Parser [Char]
    ss = many $ (char ' ' <|> char '\n')

    lock:: Char -> Parser Char
    lock c = Parser charLock where
        charLock m = case ins m of
            [] -> ParseError ""
            (x:xs) -> fn x where
                fn a| a == c = ParseError ""
                    | otherwise = Ok (x, sw xs m)

    until' :: Char -> Parser String
    until' c = many $ lock c

    -- IMPLEMENT GRAMMAR --

    --- NUMBER ---
    numChar = char '1' <|> char '2' <|> char '3' <|>
          char '4' <|> char '5' <|> char '6' <|>
          char '7' <|> char '8' <|> char '9' <|>
          char '0'

    intChar :: Parser String
    intChar = some numChar

    str2int :: String -> Int
    str2int = foldl mul 0 where
        mul a b = 10 * a + (ord b - ord '0')
    
    int :: Parser Int
    int = str2int <$> intChar

    parseNumber = int

    -- Litterals --
    parseLitterals = (Number <$> int) <|>
        (keyword "True" *> pure (Boolean True)) <|>
        (keyword "False" *> pure (Boolean False))

    -- If statements --
    parseIf = do
        keyword "if" *> ss
        cond <- parseExpr <* ss
        char ':' <* ss
        then_instr <- parseBlock <* ss
        keyword "else" <* ss  <* char ':' <* ss
        else_instr <- parseBlock <*ss
        return $ If cond then_instr else_instr

    -- Let statements --
    parseLet = do
        keyword "let" <* ss
        id <- varName <* ss
        char '=' <* ss
        value <- parseExpr
        ss
        char ';'
        ss
        return $ Let id value


    -- Var --
    nameChecker :: String -> Maybe String
    nameChecker [] = Nothing
    nameChecker (x:xs) | x == '(' = Just $ show x  ++ "not allowed in variable name"
                       | x == ')' = Just $ show x  ++ "not allowed in variable name"
                       | otherwise = nameChecker xs

    var = Parser $ \m ->
        let v = runParser (until' ' ') m in
        case v of
            Ok (a, m) -> case nameChecker a of
                Just e -> ParseError e
                Nothing -> Ok (a, m)
            ParseError e -> ParseError (show e)

    -- varName' = until' ' ' >>= nameChecker ""

    varName = handleError var "NameError : "

    parseVar = Var <$> varName


    -- Assignment --

    parseAssign = do
        v <- varName
        ss
        char '='
        ss
        e <- parseExpr
        ss
        char ';'
        ss
        return $ Assign v e

    -- Binary operator --

    parseBinOp = do
        a <- parseTerm
        ss
        op <- char '+' *> pure Sum <|> char '-' *> pure Minus <|> keyword "==" *> pure Compare
        ss
        b <- parseTerm
        return $ op a b

    parseTerm = (do
        char '('
        ss
        e <- parseExpr
        ss
        char ')'
        ss
        return e) <|> parseExpr'

    type Bob = ([Expr], Meta)

    merge:: (Expr, Meta) -> ([Expr], Meta) -> ([Expr], Meta)
    merge (x, y) (c , d) = (x : c, fn y d) where
        fn x y = case y of
            Bottom -> x
            _ -> y

    parseMore :: Meta -> Bob
    parseMore m = case runParser (parseExpr <* ss) m of
        ParseError e -> ([], Bottom)
        Ok (e, m') -> merge (e, m') (parseMore m')
    

    parseMany :: Meta -> Either [Expr] String 
    parseMany (Data "" _ _ ) = Left []
    parseMany m = case runParser (ss *> parseExpr <* ss) (trace (ins m) m) of
        ParseError e -> Right (trace "Bob error" e)
        Ok (exp, m') -> f exp (parseMany m') where 
            f a b = case (a,b) of
                (_, Right e) -> Right e
                (x, Left y) -> Left (x:y)

    getBlock = do
        char '{'
        until' '}'

    parseBlock' = Parser $ \m ->
        let s = runParser getBlock m in
            case s of 
                Ok (s, m) -> case parseMany (wrap s) of
                    Left exprs -> Ok (Block exprs, m)
                    Right e ->  ParseError e
                ParseError e -> ParseError e


    -- parseBlock = do
    --     ex <- some (parseExpr <* ss <* char ';' <* ss )
    --     return $ Block ex

    endBlock :: Parser [Expr]
    endBlock = Parser $ \m ->
        let (exprs, m') = parseMore m in
            Ok (exprs, trace ("Endblock " ++ show m') m')

    -- Block --
    -- parseBlock :: Parser Expr
    -- parseBlock = do
    --     handleError (char '{') "Missing opening bracket " <* ss
    --     blocks <- endBlock
    --     handleError (char '}') "Unclosed bracket" <* ss
    --     return $ Block blocks

    parseBlock = do
        char '{'
        ss
        ex <- some (parseExpr <* ss <* char ';' <* ss )
        char '}'
        ss
        return $ Block ex


    parseExpr = parseBinOp <|> parseLitterals <|> parseAssign <|> parseIf <|> parseLet
    parseExpr' =  parseLitterals <|> parseIf <|> parseLet <|> parseVar


    parseAll s = runParser parseBlock $ wrap s

    parse :: String -> IO ()
    parse file = do
        s <- readFile file
        print $ parseAll s