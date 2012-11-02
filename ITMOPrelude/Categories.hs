{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------

class Functor f where  
    fmap :: (a -> b) -> f a -> f b 
	
class (Functor f) => Applicative f where  
	pure :: a -> f a  
	(<*>) :: f (a -> b) -> f a -> f b  
	
class Monad m where  
	return :: a -> m a  
      
	(>>=) :: m a -> (a -> m b) -> m b  
      
	(>>) :: m a -> m b -> m b  
	x >> y = x >>= \_ -> y 
	
--------------------------------------------------------------------------------
-- Инстансы писать сюда

instance Functor Maybe where  
	fmap f (Just x) = Just (f x)  
	fmap f Nothing = Nothing
	
instance Applicative Maybe where  
	pure = Just
	Nothing <*> _ = Nothing  
	(Just f) <*> something = fmap f something
	
instance Monad Maybe where  
	return x = Just x  
	Nothing >>= f = Nothing  
	Just x >>= f  = f x 
	
--List

instance Functor List where  
	fmap = map
	
instance Applicative List where  
	pure x = Cons x Nil
	(Cons f Nil) <*> xs = map f xs
	(Cons f fs) <*> xs = (map f xs) ++ (fs <*> xs)
	
	
instance Monad List where  
	return x = Cons x Nil 
	xs >>= f = concatMap f xs
	
--Tree
instance Functor Tree where  
	fmap = treeMap
	
--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return x = State (\s -> (s, x))
    (State sa) >>= f = State (\s -> (let (newState, a) = sa s; (State g) = f a in  g newState))