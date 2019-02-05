module Lib where

-- Hide Prelude's return
import Prelude hiding (return)

-- Define "Parser" type (Our Monad)
type Parser a = String -> [(a, String)]

item :: Parser Char
item  = 
  \inp -> case inp of
    [] -> []
    (x:xs) -> [(x, xs)]

return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure  = const []

-- Apply Parser
parse :: Parser a -> String -> [(a, String)]
-- parse p inp = p inp
parse p        = \inp -> p inp
-- parse       = \p -> \inp -> p inp
-- parse       = \p -> ($ p)
-- parse       = ($)

-- Combine Parsers (Monad Composition)
(+++) :: Parser a -> Parser a -> Parser a
p +++ q
  = \inp -> case p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)]
