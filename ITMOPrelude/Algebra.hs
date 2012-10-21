module ITMOPrelude.Algebra where

class Monoid m where  
	mempty :: m  
	mappend :: m -> m -> m
	
class Monoid m => Group m where
	gempty :: m
	gappend :: m -> m -> m
	greverse :: m -> m