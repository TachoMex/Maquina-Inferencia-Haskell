import Regla
import ParteRegla
import Operador
import Atomo
import MemoriaTrabajo
import qualified Data.Map as Map
import ModuloConocimiento
import MotorInferencia
main = do
	bc <- desdeArchivoXML "ZOO" "zoo.bc" :: IO (ModuloConocimiento Bool)
	putStrLn $ show bc
	let mt = MemoriaTrabajo $ Map.fromList [] :: MemoriaTrabajo Bool
	(mt',animal) <- encandenarAdelanteIO bc mt 
	putStrLn $ show mt'
	putStrLn $ show animal
