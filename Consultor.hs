module Consultor(
	preguntarAtomo
)where
	import Data.Boolean as DB
	import Data.Char as Char
	preguntarAtomo ::(DB.Boolean t) => String -> IO t
	preguntarAtomo atomo = do
		putStrLn $ "Se cumple "++atomo++"? (S/N)"
		respuesta <- fmap (map Char.toUpper ) getLine
		case respuesta of 
			"S" -> return DB.true
			"N" -> return DB.false
			otherwise -> preguntarAtomo atomo