import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                      []    -> []
                      (c:cs) -> [(c,cs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                              []       -> []
                              [(x,cs)] -> [(g x, cs)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\inp -> [(x,inp)])

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\inp -> case parse pf inp of
                               []       -> []
                               [(f,cs)] -> parse (fmap f px) cs)

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= g = P (\inp -> case parse px inp of
                              []       -> []
                              [(x,cs)] -> parse (g x) cs)

three' :: Parser (Char, Char)
three' = do x <- item
            item
            z <- item
            return (x,z)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    px <|> py = P (\inp -> case parse px inp of
                               []       -> parse py inp
                               [(x,cs)] -> [(x,cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []     = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)

ident :: Parser String
ident = do c <- lower
           cs <- many alphanum
           return (c:cs)

nat :: Parser Int
nat = do ns <- some digit
         return (read ns)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             sat (== '\n')
             return ()

token :: Parser a -> Parser a
token p = do space
             t <- p
             space
             return t

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol cs = token (string cs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

expr :: Parser Expr
expr = do t <- term
          do symbol "+"
             e <- expr
             return (Add t e)
           <|> do symbol "-"
                  e <- expr
                  return (Sub t e)
           <|> return t

term :: Parser Expr
term = do f <- factor
          do symbol "*"
             t <- term
             return (Mul f t)
           <|> do symbol "/"
                  t <- term
                  return (Div f t)
           <|> return f

factor :: Parser Expr
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> do n <- natural
                 return (Val n)

eval :: String -> Expr
eval cs = case (parse expr cs) of
               [(n,[])]  -> n
               [(_,out)] -> error ("Unused input " ++ out)
               []        -> error "Invalid input"

data Expr =
    Val Int |
    Add Expr Expr |
    Sub Expr Expr |
    Mul Expr Expr |
    Div Expr Expr
    deriving Show
