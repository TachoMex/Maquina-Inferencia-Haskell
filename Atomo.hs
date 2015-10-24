module Atomo(
	Atomo(..)
)where
	import qualified Data.Boolean as DB
	data Atomo t = Atomo{
		descripcion :: String,
		valor       :: t,
		objetivo    :: Bool
	}


	instance (DB.Boolean t, Show t) => Show (Atomo t) where
		show (Atomo desc v True)= '(':'*':desc++":"++(show v)++")" 
		show (Atomo desc v False)= '(':desc++":"++(show v)++")" 

	instance (DB.Boolean t) => Eq (Atomo t) where
		(Atomo a _ _) == (Atomo b _ _) = a == b
		(Atomo a _ _) /= (Atomo b _ _) = a /= b 

	instance (DB.Boolean t) => Ord (Atomo t) where
		(Atomo a _ _) < (Atomo b _ _) = a < b 
		(Atomo a _ _) <= (Atomo b _ _) = a <= b 
		(Atomo a _ _) > (Atomo b _ _) = a > b 
		(Atomo a _ _) >= (Atomo b _ _) = a >= b 

	