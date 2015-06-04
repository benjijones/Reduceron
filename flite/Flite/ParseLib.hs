{-# LANGUAGE DeriveFunctor #-}

module Flite.ParseLib where


import Data.Char
import Data.Tuple
import Control.Applicative

infixl 5 <|
infixl 6 |>

newtype Parser a = Parser { unParser :: (String -> [(String, a)]) }
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \s -> [(s, a)]
  f <*> a = Parser $ \s -> [(s1, g b) | (s0, g) <- unParser f s, (s1, b) <- unParser a s0]

instance Alternative Parser where
  empty = Parser $ (\s -> [])
  p <|> q = Parser $ (\s -> let p1 = unParser p s in
                                if null p1 
                                then p1
                                else unParser q s)
  
(|>) :: Parser a -> Parser b -> Parser b
a |> b = pure (\a b -> b) <*> a <*> b

(<|) :: Parser a -> Parser b -> Parser a
a <| b = pure (\a b -> a) <*> a <*> b

guarded :: (a -> Bool) -> Parser a -> Parser a
guarded f p = Parser $ \s -> [(s', a) | (s', a) <- unParser p s, f a]

sat :: (Char -> Bool) -> Parser Char
sat f = Parser satisfy 
  where
      satisfy "" = []
      satisfy (c:s) = [(s, c) | f c]

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string "" = pure ""
string (c:cs) = pure (:) <*> char c <*> string cs

alphanum :: Parser Char
alphanum = sat isAlphaNum

digit :: Parser Int
digit = pure (\c -> ord c - ord '0') <*> sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

many1 :: Parser a -> Parser [a]
many1 p = pure (:) <*> p <*> many p

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = p <| space

tok :: String -> Parser String
tok = token . string

nat :: Parser Int
nat = token natural

natural :: Parser Int
natural = pure total <*> many1 digit
  where total = foldl (\acc n -> 10*acc + n) 0

int :: Parser Int
int = token integer

integer :: Parser Int
integer = natural <|> pure negate <*> (char '-' |> natural)

strLit :: Parser String
strLit = Parser $ strLit'
  where strLit' s@('"':_) = map swap (lex s)
        strLit _ = []

charLit :: Parser String
charLit = Parser $ charLit'
  where charLit' s@('\'':_) = map swap (lex s)
        charLit' _ = []

parse :: Parser a -> String -> a
parse p s =
  case unParser p s of
    []        -> error "Parse error"
    [("", x)] -> x
    [(s, x)]  -> error "Parse error"
    _         -> error "Ambiguous parse --- this shouldn't happen!"
