{-# LANGUAGE NoTemplateHaskell #-}
module Monstupar.Derived where

-- � ���� ������ �� ����� ��������� ���� Monstupar ��
import Monstupar.Core
-- ��������,
-- blah = Monstupar $ undefined
-- �� ��������������, ��������� ����������� Monstupar ���������,
-- ������� �������������� ������� ��� ����� ������ ��������� �����������
-- ������� �� Core.

--------------------------------------------------------------------------------
-- ������ ������� � �������� �����

-- �� �����
notok :: Monstupar s ()
notok = isnot ok

-- � ������ ����� ������ � �������� s
char :: Eq s => s -> Monstupar s s
char = like . (==)

-- � ������ ����� ������ ���-�� �� ������
oneOf :: Eq s => [s] -> Monstupar s s
oneOf xs = like (\x -> x `elem` xs)

-- � �������� ������ ������ ����� ������ �����������
string :: Eq s => [s] -> Monstupar s [s]
string [] = return []
string (x:xs) = do
	x' <- char x
	xs' <- string xs
	return (x':xs')

-- "��������" -- ��������� ������ ������������ (���� ��� �����) ����� ��� �
-- ��������������� ����������
many :: Monstupar s a -> Monstupar s [a]
many p = do
	e <- p
	es <- many p
	return (e:es) <|> return []
-- ��������� � �����������! ������� �� ���, ����� � ��� ��-�� ������������� <|>
-- �� ��� � ������������� ����.

-- "������" -- ���� ��� ����� ���
many1 :: Monstupar s a -> Monstupar s [a]
many1 p = do
    e <- p
    es <- many p
    return (e:es)

-- "��������" -- ���� ��� ���� ���
optional :: Monstupar s a -> Monstupar s (Maybe a)
optional p = do 
	e <- p
	return (Just e) <|> return Nothing