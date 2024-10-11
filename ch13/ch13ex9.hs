import Control.Applicative
import Data.Char
import System.IO

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
    return = pure
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

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol cs = token (string cs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

anyChar :: String -> Parser Char
anyChar cs = token (anyChar' cs) where
    anyChar' [] = empty
    anyChar' (c:cs) = do char c
                         return c
                       <|> anyChar' cs

-- expr ::= term ((+|-) term)*
expr :: Parser Int
expr = do t <- term
          do 
              ts <- many (do op <- anyChar "+-"
                             t' <- term
                             return (if op == '+' then (op,t') else (op,t')) )
              return (foldl (\acc (op,x) -> if op == '+' then (acc + x) else (acc - x)) t ts)
           <|> return t

term :: Parser Int
term = do p <- pow
          do
              ps <- many (do op <- anyChar "*/"
                             p' <- pow
                             return (if op == '*' then (op,p') else (op,p')) )
              return (foldl (\acc (op,x) -> if op == '*' then (acc * x) else (acc `div` x)) p ps)
           <|> return p

pow :: Parser Int
pow = do f <- factor
         do symbol "^"
            p <- pow
            return (f ^ p)
          <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> integer

{-  
eval :: String -> Int
eval cs = case (parse expr cs) of
               [(n,[])]  -> n
               [(_,out)] -> error ("Unused input " ++ out)
               []        -> error "Invalid input"
-}

type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
              standard = "qcd=123+456-789*0()/"
              extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display :: String -> IO ()
display xs = do writeat (3,2) (replicate 13 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                 process c xs
             else
                 do beep
                    calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = eval xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
              [(n,[])]  -> calc (show n)
              [(_,out)] -> do writeat (1,14) ("Unexpected characters: " ++ out)
                              getCh
                              calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
