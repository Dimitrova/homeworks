{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже

class Monoid m where  
	mempty :: m  
	mappend :: m -> m -> m
	
class Monoid m => Group m where
	gempty :: m
	gappend :: m -> m -> m
	greverse :: m -> m
	
------
instance Monoid Bool where
	mempty = True
	mappend = (&&)
	
data OrBool = Or Bool

instance Monoid OrBool where
	mempty = Or False
	mappend (Or a) (Or b) = Or (a || b)
	
instance Monoid Nat where
	mempty = Zero
	mappend = (+.)
	
data MulNat = MulN Nat

instance Monoid MulNat where
	mempty = MulN Zero
	mappend (MulN a) (MulN b) = MulN (a *. b)

instance Monoid Int where
	mempty = IntZero
	mappend = (.+.)
	
instance Group Int where
	gempty = IntZero
	gappend = (.+.)
	greverse = intNeg
	
data MulInt = MulI Int

instance Monoid MulInt where
	mempty = MulI intOne
	mappend (MulI a) (MulI b) = MulI (a .*. b)
	
instance Monoid Rat where
	mempty = Rat IntZero natOne
	mappend = (%+)
	
instance Group Rat where
	gempty = Rat IntZero natOne
	gappend = (%+)
	greverse = ratNeg
	
data MulRat = MulR Rat

instance Monoid MulRat where
	mempty = MulR (Rat intOne natOne)
	mappend (MulR a) (MulR b) = MulR (a %* b)
	
instance Group MulRat where
	gempty = MulR (Rat IntZero natOne)
	gappend (MulR a) (MulR b) = MulR (a %* b)
	greverse (MulR a) = MulR (ratInv a)