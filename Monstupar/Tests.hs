module Monstupar.Tests where

import Monstupar.Core
import Monstupar.Derived

--------------------------------------------------------------------------------
-- В помощь хозяйке

mustParse s p = case runParser p s of
    Left _ -> False
    Right _ -> True

mustFail s = not . mustParse s

infixl 2 &.&
(&.&) p1 p2 x = p1 x && p2 x

--------------------------------------------------------------------------------
-- Тесты

-- Правильная скобочная последовательность
balPar = bp >> eof where
    bp = (do
          char '('
          bp
          char ')'
          bp) <|> ok

balParTest = mustParse ""
         &.& mustFail "("
         &.& mustFail ")"
         &.& mustParse "()"
         &.& mustParse "(())()(())()"
         &.& mustFail "())()(())()"
         &.& mustFail "(())()(()()"
         &.& mustFail "())()(()()"
         $ balPar

-- Список натуральных чисел
-- тут следует использовать класс Read
digit = oneOf ['0'..'9']
digits = many1 digit

natList :: Monstupar Char [String]
natList = (do 
		   x <- nl
		   eof
		   return x) where
	nl = (do
		  n <- digits
		  char ','
		  ns <- nl
		  return (n:ns)) <|> 
		 (do
		  n <- digits
		  return [n])

natListTest = mustFail ""
          &.& mustParse "0"
          &.& mustParse "0,1"
          &.& mustFail "0,1,"
          &.& mustParse "10,20,12,3423,2342,234,2234,2342,22342,22232,17583,9573"
          &.& mustFail "10,20,12,3423,2342,234,-2234,2342,22342,22232,17583,9573"
          &.& mustFail "10,20,12,3423,0.,234,234,2342,22342,22232,17583,9573"
          $ natList
		  
--Парсер для типа стрелочка
data Type = TVar String | Arrow Type Type deriving (Eq)
		
dataType :: Monstupar Char Type
dataType = bp where
    bp = (do
			spaces
			char '('
			spaces
			type' <- bp
			spaces
			char ')'
			spaces
			return type') <|>
		 (do
			spaces
			var <- letters
			spaces
			char '-'
			char '>'
			spaces
			type' <- bp
			return (Arrow (TVar var) type')) <|>
		 (do
			spaces
			var <- letters
			return (TVar var))
			
test1 = case runParser dataType "a -> b -> c" of
	Left x -> error "e"
	Right (_, x) -> if (x == (Arrow (TVar "a") (Arrow (TVar "b") (TVar "c")))) then True else False
	
test2 = case runParser dataType "a -> (b -> c)" of
	Left x -> error "e"
	Right (_, x) -> if (x == (Arrow (TVar "a") (Arrow (TVar "b") (TVar "c")))) then True else False
	
test3 = case runParser dataType "a" of
	Left x -> error "e"
	Right (_, x) -> if (x == TVar "a") then True else False