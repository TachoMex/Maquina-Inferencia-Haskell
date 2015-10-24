module MotorInferencia (
	encandenarAdelanteIO
)where
	import Regla
	import Operador
	import Bool
	import Atomo
	import Data.Boolean as DB
	import MemoriaTrabajo
	import ModuloConocimiento

	encandenarAdelanteIO :: (DB.Boolean t, Show t, Eq t) => (ModuloConocimiento t) -> (MemoriaTrabajo t) -> IO (MemoriaTrabajo t, Maybe (Atomo t))
	encandenarAdelanteIO (ModuloConocimiento mc _) mt = encandenarAdelanteIO' mc mt

	encandenarAdelanteIO' :: (DB.Boolean t, Show t, Eq t) => [Regla t] -> (MemoriaTrabajo t) -> IO (MemoriaTrabajo t,Maybe (Atomo t))
	encandenarAdelanteIO' [] mt  = return (mt,Nothing)
	encandenarAdelanteIO' (regla : mc) mt =	do
		putStrLn $ "Probando regla: "++(show regla)
		putStrLn $ show mt
		putStrLn $ show $ Regla.objetivo regla
		(mt',activacion) <- evaluaIO regla mt
		putStrLn $ show activacion
		case activacion of
			(Just value) -> if Regla.objetivo regla && (value == DB.true) then 
				return (mt', Just $ atomoConclusion regla) 
				else if value == DB.true then
				encandenarAdelanteIO' mc $ dispara regla mt' 
				else
				encandenarAdelanteIO' mc mt'
			Nothing -> encandenarAdelanteIO' mc mt'

