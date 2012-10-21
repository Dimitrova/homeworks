{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.Category

-- Определение

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show,Read)

-- Создание пустого дерева
emptyTree :: a -> Tree a 
emptyTree _ = Empty

-- Добавление элемента в вершину дерева
addNode :: a -> Tree a -> Tree a
addNode a b = Node a b Empty

--Добавление самого левого элемента
addLeft :: a -> Tree a -> Tree a
addLeft a Empty = Node a Empty Empty
addLeft a (Node b l r) = Node b (addLeft a l) r

--Добавление самого правого элемента
addRight :: a -> Tree a -> Tree a
addRight a Empty = Node a Empty Empty
addRight a (Node b l r) = Node b l (addRight a r)

--Левый поворот
turnLeft :: Tree a -> Tree a
turnLeft (Node a l Empty) = error "turnLeft: wrong tree"
turnLeft (Node a l (Node b l1 r1)) = Node b (Node a l l1) r1

--Правый поворот
rightTurn :: Tree a -> Tree a
rightTurn (Node _ Empty _) = error "turnRight: wrong tree"
rightTurn (Node a (Node b l1 r1) r) = Node b l1 (Node a r1 r)

--Аналог функции map
treeMap:: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Node a Empty Empty) = Node (f a) Empty Empty
treeMap f (Node a l r) = Node (f a) (treeMap f l) (treeMap f r)

--Аналог функции foldr
treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr _ z Empty = z
treeFoldr f z (Node a l r) = f a (treeFoldr f (treeFoldr f z r) l) 

instance Functor Tree where  
	fmap = treeMap