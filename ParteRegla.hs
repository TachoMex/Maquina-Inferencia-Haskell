module ParteRegla(
	ParteRegla(..)
) where
	import Operador
	import Atomo
	import Bool
	import qualified Data.Boolean as DB
	data ParteRegla t = PRO Operador | PRA (Atomo t) 

	instance (DB.Boolean t, Show t) => Show (ParteRegla t) where
		show (PRO op) = show op
		show (PRA at) = show at