{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- ���
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- ������ ���
instance Monad m => MonadJoin m where
		returnJoin = return
		join mma = mma >>= (\id -> id)
		
instance Monad m => MonadFish m where
	returnFish = return
	g >=> f = \a -> (g a >>= f)
	
instance Monad m => Functor m where
	fmap g fa = fa >>= (\a -> return (g a))