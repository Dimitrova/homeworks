{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import qualified Prelude as P ((++))
import ITMOPrelude.Primitive

---------------------------------------------
-- ��� ���� ������?
--
-- ��� undefined ���������� � ��������� �����.
-- ��������� (*) �������� �����, � ������� ����� ������������� ������.

---------------------------------------------
-- �����������

data List a = Nil | Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- ��������

-- ����� ������
length :: List a -> Nat
length Nil = Zero
length (Cons x xs) = Succ (length xs)

-- ������� ��� ������ �� O(length a)
(++) :: List a -> List a -> List a
Nil ++ a = a
(Cons x xs) ++ a = Cons x (xs ++ a)

-- ������ ��� ������� ��������
tail :: List a -> List a
tail Nil = error "tail: Null pointer exception"
tail (Cons x xs)  = xs

-- ������ ��� ���������� ��������
init :: List a -> List a
init Nil = error "init: Null pointer exception"
init (Cons x xs) = case (natEq (length xs) Zero) of
	True -> Nil
	False -> Cons x (init xs)

-- ������ �������
head :: List a -> a
head Nil = error "head: Null pointer exception"
head (Cons x xs) = x

-- ��������� �������
last :: List a -> a
last Nil = error "last: Null pointer exception"
last (Cons x xs) = case (natEq (length xs) Zero) of
	True -> x
	False -> last xs

-- n ������ ��������� ������
take :: Nat -> List a -> List a
take _ Nil = Nil
take Zero _ = Nil
take (Succ n) (Cons x xs) = Cons x (take n xs)

-- ������ ��� n ������ ���������
drop :: Nat -> List a -> List a
drop _ Nil = Nil
drop Zero a = a
drop (Succ n) (Cons x xs) = drop n xs

-- �������� � ������ ������ �������� ��������������� p
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons x xs) = case (p x) of
	True -> Cons x (filter p xs)
	False -> filter p xs

-- ���������� ������. ������ "���������/��������" p
-- ������� "���������/�������� b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons x xs) = case (p x) of
	Just y -> Cons y (gfilter p xs)
	_ -> gfilter p xs

-- ���������� �� ������ � ��������� �� ������� ��������� ���������
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons x xs) = case (p x) of
	True -> Cons x (takeWhile p xs)
	False -> Nil

-- �� ���������� �� ������ � ��������� �� ������� ��������� ���������,
-- ����� ���� ����������� ��� ��������, ������� ������ ����������
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p (Cons x xs) = case (p x) of
	True -> dropWhile p xs
	False -> (Cons x xs)

-- ������� ������ �� ��������� �� (takeWhile p xs, dropWhile p xs),
-- �� �����������
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ Nil = Pair Nil Nil
span p (Cons x xs) = case (p x) of 
	True -> (let (Pair a b) = span p xs in Pair (Cons x a) b)
	False -> Pair Nil (Cons x xs)
	
-- ������� ������ �� ��������� �� (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- �� �����������
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a

-- n-�� ������� ������ (������ � ����)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs) !! Zero = x
(Cons x xs) !! (Succ n) = xs !! n

-- ������ ����� �� ����
reverse :: List a -> List a
reverse Nil = Nil
reverse a = (Cons (last a) (init a)) ++ (Cons (head a) Nil)

-- (*) ��� ��������� ������� ������
subsequences :: List a -> List (List a)
subsequences Nil = Nil
subsequences (Cons x xs) = (subsequences xs) ++ (permutations (Cons x xs))

-- (*) ��� ������������ ��������� ������� ������
permutations :: List a -> List (List a)
permutations Nil = Nil
permutations (Cons x xs) = (let res = permutations xs in map (Cons x) res)

-- (*) ���� ������. ��� ������������ ��������� ������� ������
-- ������ ��������
permutations' :: List a -> List (List a)
permutations' = undefined

-- ��������� ������� ����������� ����� ���
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- ����� ������
-- ��������� ����� ������ ����������:
-- f
-- / \
-- f ...
-- / \
-- f l!!2
-- / \
-- f l!!1
-- / \
-- z l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ z Nil = z
foldl f z (Cons x xs) = foldl f (f z x) xs

-- ��� �� foldl, �� � ������ ����������� ��� ������������� ����������
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl _ z Nil = Cons z Nil
scanl f z (Cons x xs) = (let a = f z x in Cons a (scanl f a xs))

-- ������ ������
-- ��������� ����� ������ ����������:
-- f
-- / \
-- l!!0 f
-- / \
-- l!!1 f
-- / \
-- l!!2 ...
-- \
-- z
-- 
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil = z
foldr f z (Cons x xs) = f x (foldr f z xs)

-- ����������
-- head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ z Nil = Cons z Nil
scanr f z (Cons x xs) = (let (Cons a b) = scanr f z xs in Cons (f x a) (Cons a b))

-- ������ ����������� �� �������� �����
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- ��������� f � ������� �������� ������
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x)(map f xs)

-- ��������� ������ ������� � ������
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons x xs) = x ++ (concat xs)

-- ���������� (concat . map), �� �����������
concatMap :: (a -> List b) -> List a -> List b
concatMap _ Nil = Nil
concatMap f (Cons x xs) = (f x) ++ (concatMap f xs)

-- �������� ��� ������ � ������ ��� ������ min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) (zip xs ys)

-- ����������, �� ������� ��� ������ �������, � �� ������������� Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)