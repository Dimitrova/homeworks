{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- делаем из нас
instance MonadFish m => Monad m where
    return = returnFish
    ma >>= f = ((\id -> id) >=> f) ma

instance MonadFish m => Functor m where
    fmap g fa = ((\id -> id) >=> (\a -> returnFish (g a))) fa

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join mma = ((\id -> id) >=> (\id -> id)) mma