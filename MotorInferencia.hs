module MotorInferencia (
	encandenarAdelanteIO,
	encandenarAdelanteIOf
)where
	import Regla
	import Operador
	import Bool
	import Atomo
	import Data.Boolean as DB
	import MemoriaTrabajo
	import ModuloConocimiento
	import Consultor as Cons

	encandenarAdelanteIO :: (DB.Boolean t, Show t, Eq t) => (ModuloConocimiento t) -> (MemoriaTrabajo t) ->Bool ->IO (MemoriaTrabajo t, Maybe (Atomo t))
	encandenarAdelanteIO (ModuloConocimiento mc _) mt disparaNegativas = encandenarAdelanteIO' mc mt Cons.preguntarAtomo disparaNegativas

	encandenarAdelanteIOf (ModuloConocimiento mc _) mt preguntarAtomo disparaNegativas = encandenarAdelanteIO' mc mt preguntarAtomo disparaNegativas

--	encandenarAdelanteIO' :: (DB.Boolean t, Show t, Eq t) => [Regla t] -> (MemoriaTrabajo t) -> IO (MemoriaTrabajo t,Maybe (Atomo t))
	encandenarAdelanteIO' [] mt  _ disparaNegativas= return (mt,Nothing)
	encandenarAdelanteIO' (regla : mc) mt preguntarAtomo disparaNegativas = do
		putStrLn $ "Probando regla: "++(show regla)
		putStrLn $ show mt
		putStrLn $ show $ Regla.objetivo regla
		(mt',activacion) <- evaluaIOf regla mt preguntarAtomo
		putStrLn $ show activacion
		case activacion of
			(Just value) -> if Regla.objetivo regla && (value == DB.true) then 
				return (mt', Just $ atomoConclusion regla) 
				else if value == DB.true then
				encandenarAdelanteIO' mc (dispara regla mt') preguntarAtomo disparaNegativas
				else
				if disparaNegativas then encandenarAdelanteIO' mc (disparaNegadas regla mt') preguntarAtomo disparaNegativas else  encandenarAdelanteIO' mc mt' preguntarAtomo disparaNegativas
			Nothing -> 
				if disparaNegativas then encandenarAdelanteIO' mc (disparaNegadas regla mt') preguntarAtomo disparaNegativas else  encandenarAdelanteIO' mc mt' preguntarAtomo disparaNegativas

