module Parser where
    import Control.Monad
    import qualified Control.Monad as Control
    import qualified Control.Applicative as Control.Monad
    newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

    instance Control.Monad.Functor Parser where
        fmap f p = Parser $ \s -> do
            (o, s') <- runParser p s
            return (f o, s')

    instance Control.Monad.Applicative Parser where
        pure x = Parser $ \s -> Just (x, s)

        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        Parser f <*> Parser a = Parser $ \s -> do
            (f', s') <- f s
            (o, s'') <- a s'
            return (f' o, s'')

    instance Control.Monad.Monad Parser where
        (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        Parser p >>= f = Parser $ \s -> do
            (o, s') <- p s
            (o', s'') <- (runParser $ f o) s'
            return (o', s'')

    instance Control.Monad.Alternative Parser where
        empty = Parser $ const Nothing
        Parser a <|> Parser b = Parser $ \s ->
            case a s of
                Just (o, s') -> Just (o, s')
                Nothing -> b s