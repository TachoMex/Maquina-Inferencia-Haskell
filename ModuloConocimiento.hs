module ModuloConocimiento(
	ModuloConocimiento(..),
	desdeArchivoXML,
	agregarRegla
)where
	import Regla
	import Operador
	import Bool
	import Atomo
	import qualified Data.Boolean as DB
	import MemoriaTrabajo
	data ModuloConocimiento t = ModuloConocimiento{
		reglas :: [Regla t],
		descripcion :: String
	} 

	desdeArchivoXML ::(DB.Boolean t) => String -> String ->IO (ModuloConocimiento t)
	desdeArchivoXML desc nom_arch = do 
		bc <- readFile nom_arch
		let lineas = lines bc 
		let reglas = map analizaXML lineas
		return $ ModuloConocimiento reglas desc

	agregarRegla :: (DB.Boolean t) => (ModuloConocimiento t) -> (Regla t) -> (ModuloConocimiento t)
	agregarRegla (ModuloConocimiento reglas desc) regla = (ModuloConocimiento (regla:reglas) desc)

	instance (Show t, DB.Boolean t) => Show (ModuloConocimiento t) where
	 	show (ModuloConocimiento reglas desc) =  "Modulo de Conocimiento: "++desc++"\n"++reglas_str
	 		where
	 			reglas_str = unlines $ map show reglas