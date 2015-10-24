module Operador(
	Operador(..)
) where 
	data Operador = Conjuncion | Negacion | Disyuncion 

	instance Show Operador where
		show Conjuncion = "&"
		show Disyuncion = "|"
		show Negacion   = "!" 