{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import qualified Prelude as P ((++))
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil | Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons x xs) = Succ (length xs)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ a = a
(Cons x xs) ++ a = Cons x (xs ++ a)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "tail: Null pointer exception"
tail (Cons x xs)  = xs

-- Список без последнего элемента
init :: List a -> List a
init Nil = error "init: Null pointer exception"
init (Cons x xs) = case (natEq (length xs) Zero) of
	True -> Nil
	False -> Cons x (init xs)

-- Первый элемент
head :: List a -> a
head Nil = error "head: Null pointer exception"
head (Cons x xs) = x

-- Последний элемент
last :: List a -> a
last Nil = error "last: Null pointer exception"
last (Cons x xs) = case (natEq (length xs) Zero) of
	True -> x
	False -> last xs

-- n первых элементов списка
take :: Nat -> List a -> List a
take _ Nil = Nil
take Zero _ = Nil
take (Succ n) (Cons x xs) = Cons x (take n xs)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop _ Nil = Nil
drop Zero a = a
drop (Succ n) (Cons x xs) = drop n xs

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons x xs) = case (p x) of
	True -> Cons x (filter p xs)
	False -> filter p xs

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons x xs) = case (p x) of
	Just y -> Cons y (gfilter p xs)
	_ -> gfilter p xs

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons x xs) = case (p x) of
	True -> Cons x (takeWhile p xs)
	False -> Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p (Cons x xs) = case (p x) of
	True -> dropWhile p xs
	False -> (Cons x xs)

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ Nil = Pair Nil Nil
span p (Cons x xs) = case (p x) of 
	True -> (let (Pair a b) = span p xs in Pair (Cons x a) b)
	False -> Pair Nil (Cons x xs)
	
-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs) !! Zero = x
(Cons x xs) !! (Succ n) = xs !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse Nil = Nil
reverse a = (Cons (last a) (init a)) ++ (Cons (head a) Nil)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Nil
subsequences (Cons x xs) = (subsequences xs) ++ (permutations (Cons x xs))

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Nil
permutations (Cons x xs) = (let res = permutations xs in map (Cons x) res)

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
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

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl _ z Nil = Cons z Nil
scanl f z (Cons x xs) = (let a = f z x in Cons a (scanl f a xs))

-- Правая свёртка
-- порождает такое дерево вычислений:
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

-- Аналогично
-- head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ z Nil = Cons z Nil
scanr f z (Cons x xs) = (let (Cons a b) = scanr f z xs in Cons (f x a) (Cons a b))

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x)(map f xs)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons x xs) = x ++ (concat xs)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap _ Nil = Nil
concatMap f (Cons x xs) = (f x) ++ (concatMap f xs)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) (zip xs ys)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)