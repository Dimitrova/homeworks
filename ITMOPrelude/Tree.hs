{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.Category

-- �����������

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show,Read)

-- �������� ������� ������
emptyTree :: a -> Tree a 
emptyTree _ = Empty

-- ���������� �������� � ������� ������
addNode :: a -> Tree a -> Tree a
addNode a b = Node a b Empty

--���������� ������ ������ ��������
addLeft :: a -> Tree a -> Tree a
addLeft a Empty = Node a Empty Empty
addLeft a (Node b l r) = Node b (addLeft a l) r

--���������� ������ ������� ��������
addRight :: a -> Tree a -> Tree a
addRight a Empty = Node a Empty Empty
addRight a (Node b l r) = Node b l (addRight a r)

--����� �������
turnLeft :: Tree a -> Tree a
turnLeft (Node a l Empty) = error "turnLeft: wrong tree"
turnLeft (Node a l (Node b l1 r1)) = Node b (Node a l l1) r1

--������ �������
rightTurn :: Tree a -> Tree a
rightTurn (Node _ Empty _) = error "turnRight: wrong tree"
rightTurn (Node a (Node b l1 r1) r) = Node b l1 (Node a r1 r)

--������ ������� map
treeMap:: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Node a Empty Empty) = Node (f a) Empty Empty
treeMap f (Node a l r) = Node (f a) (treeMap f l) (treeMap f r)

--������ ������� foldr
treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr _ z Empty = z
treeFoldr f z (Node a l r) = f a (treeFoldr f (treeFoldr f z r) l) 

instance Functor Tree where  
	fmap = treeMap