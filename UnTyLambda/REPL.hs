{-# LANGUAGE ScopedTypeVariables #-}
-- REPL for untyped lambda calculus
module UnTyLambda.REPL where

import Prelude hiding (catch)
import Monstupar
import UnTyLambda.Interpreter
import Data.List

-- ������ ������ � ����
parseLambda :: Monstupar Char Term
parseLambda = (do
				term <- parseTerm
				spaces
				terms <- parseLambda
				return (App term terms)) <|>
			  (do 
				term <- parseTerm
				return term)
				
parseTerm = (do
				char '\\'
				spaces
				var <- parseVarName
				spaces
				char '.'
				spaces
				f <- parseTerm
				spaces
				return (Lam var f)) <|>
			(do
				char '\\'
				spaces
				var <- parseVarName
				spaces
				char '.'
				spaces
				f <- parseName
				spaces
				return (Lam var f))

parseBraces = do
				char '('
				spaces
				c <- parseLambda
				char ')'
				return c

parseName = do
				name <- parseVarName
				return (Var name)
				
parseVarName = do
				c <- letter
				cs <- many (letter <|> digit)
				return (c:cs)
				
letter = oneOf (['a'..'z'] ++ ['A'..'Z'])
digit = oneOf ['0'..'9']
spaces = many1 (oneOf [' ', '\n', '\r'])
--------------------------------------------------------------------------------
-- �������, ��� ���������� ������-��������� ��������������.
-- ����� ��� ��� ��������� ������, ������� ������� �����������������
-- (��������������� ���� ����� ������) �� ������, � ����� ��������
-- ������������ ���������� � EBNF ��� ����:
-- Term = Var | Lam | ( Term )
-- Lam = \Var . Term | ( Lam ) | Var
-- Var = letter (letter | digit)*
-- ����� ����, ��
--------------------------------------------------------------------------------

-- ������� �������� ���� (����� � ������� ��������, ����� ���)
prettyPrint :: Term -> String
prettyPrint (Var v) = "( " ++ v ++ " )"
prettyPrint (Lam v t) = "(\\ " ++ v ++ " . " ++ (prettyPrint t) ++ ")"
prettyPrint (App t t') = (prettyPrint t) ++ " " ++ (prettyPrint t')

-- ���������� ��� REPL. ������ �������� � ������������ ����� �������� ���
-- ������� ������������ ���������� �� ������� ���������.
replLoop :: Integer -> (Integer -> Term -> Term) -> IO ()
replLoop patience strategy = do 
								putStr "> "
								line <- getLine
								case runParser parseLambda line of
									Left _ -> putStrLn "Parse error"
									Right (_, t) -> do
														putStrLn (prettyPrint (strategy patience t))
														replLoop (patience - 1) strategy

-- ������ � (replLoop 100 no) ������ ��������� ���:
-- > \x . (\y . y) x x
-- \x . x x