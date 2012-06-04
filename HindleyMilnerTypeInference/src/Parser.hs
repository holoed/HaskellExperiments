module Parser where

import Prelude (Monad, String, concat, return, (>>=), Char, Bool, Int, (==), (-), (+), (*), div)
import Data.Char (isSpace, isDigit, ord)
  
-- Monadic Parsing in Haskell
-- Graham Hutton - University of Nottingham
-- Erik Meijer - University of Utrecht

data Parser a = Parser (String -> [(a, String)])

unit :: a -> Parser a
unit x = Parser (\s -> [(x, s)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser m) s = m s

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser m) f = Parser (\s -> concat [parse (f x) s' | (x, s') <- m s])
                                    
instance Monad Parser where
  return x = unit x
  m >>= f = bind m f

item :: Parser Char
item = Parser (\s -> case s of 
                     "" -> []
                     x:xs -> [(x, xs)])     
                     
zero :: Parser a
zero = Parser (\_ -> [])                         
                     
(++) :: Parser a -> Parser a -> Parser a
p ++ q = Parser (\s -> concat [(parse p s), (parse q s)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\s -> case parse (p ++ q) s of
                          [] -> []
                          x:_ -> [x])   
                          
sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- item; if p c then return c else zero }    

char :: Char -> Parser Char
char c = sat (c ==)      

-- Recursion combinators                                                             
              
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {x <- char c; xs <- string cs; return (x:xs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {x <- p; xs <- many p; return (x:xs)}    

sepBy :: Parser a -> Parser a -> Parser [a]
p `sepBy` sep = p `sepBy1` sep +++ return []

sepBy1 :: Parser a -> Parser a -> Parser [a]
p `sepBy1` sep = do x <- p
                    xs <- many (do { _ <- sep; p })
                    return (x:xs)                  
                    
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
                 where
                    rest a = (do f <- op
                                 b <- p
                                 rest (f a b))
                             +++ return a             
                             
-- Lexical combinators                       

space :: Parser String
space = many (sat isSpace)    

token :: Parser a -> Parser a
token p = do { a <- p; _ <- space; return a }    

symb :: String -> Parser String
symb cs = token (string cs)             

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {_ <- space; p})        

-- Example

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainl1` mulop

factor :: Parser Int
factor = digit +++ do { _ <- symb "("; n <- expr; _ <- symb ")"; return n }

digit :: Parser Int
digit = do { x <- token (sat isDigit); return (ord x - ord '0')}

addop :: Parser (Int -> Int -> Int)
addop = do { _ <- symb "+" ; return (+)} +++ do { _ <- symb "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do { _ <- symb "*" ; return (*)} +++ do { _ <- symb "/"; return (div)}

