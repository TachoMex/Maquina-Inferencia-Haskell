module Bool(
	FuzzyBool(..),
	Probabilidad(..)
)where
	import qualified Data.Boolean as DB
	newtype FuzzyBool =  FuzzyBool Double
	instance Show FuzzyBool where
		show (FuzzyBool p) = "fuzzy "++(show p) 

	instance DB.Boolean FuzzyBool where
		(&&*) (FuzzyBool x) (FuzzyBool y) = (FuzzyBool $ min x y)
		(||*) (FuzzyBool x) (FuzzyBool y) = (FuzzyBool $ max x y)
		true = (FuzzyBool 1.0)
		false = (FuzzyBool 0.0)
		notB (FuzzyBool x) = (FuzzyBool $ 1.0 - x)

	newtype Probabilidad = Probabilidad Double
	instance Show Probabilidad where
	 	show (Probabilidad p) = "Probabilidad "++(show p) 

	instance DB.Boolean Probabilidad where
	 	(&&*) (Probabilidad x) (Probabilidad y) = (Probabilidad $ x * y) 
	 	(||*) (Probabilidad x) (Probabilidad y) = (Probabilidad $ min 1.0 $ x + y)
	 	true = (Probabilidad 1.0)
	 	false = (Probabilidad 0.0)
	 	notB (Probabilidad x) = (Probabilidad $ 1.0 - x)