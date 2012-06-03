module Parser where

import Prelude (Monad, String, concat, return, (>>=), Char, Bool, (==))
  
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
              
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {x <- p; xs <- many p; return (x:xs)}                                     