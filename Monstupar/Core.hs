-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- �����������

-- ���� ����� ����������� ����� �������� �� ��, ��� ���������
data ParseError = ParseError String
                deriving (Show) -- ���� �� show ���

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s , a)
    ma >>= f = Monstupar (\s ->  case runParser ma s of
		Left err -> Left err
		Right (s', a) -> runParser (f a) s')

--------------------------------------------------------------------------------
-- ����������� �������.
-- ����� � ��������� ������� ������ ������, ���� �����

-- �� ������
ok :: Monstupar s ()
ok = Monstupar $ \s -> Right (s , ())

-- �� ������ ��������� �������� p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
    Left e -> Right (s , ())
    Right _ -> Left (ParseError "must not parse")

-- ����� �����
eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s , ())
    _ -> Left (ParseError "end of file")

infixr 2 <|>
-- ������� ������ ������, ���� �� ��������, �� ������
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
	Left _ -> runParser b s
	Right _ -> runParser a s

-- � ������ ����� ������ �����, ��������������� p
like :: (s -> Bool) -> Monstupar s s
like p = Monstupar $ \s -> case s of
	[] -> Left (ParseError "empty list")
	(x:xs) -> case p x of
		True -> Right (xs, x)
		False -> Left (ParseError "not like p")

-- ���� ����� ��������� ��� �����-�� ����������� �������
-- ���� ��� �����������