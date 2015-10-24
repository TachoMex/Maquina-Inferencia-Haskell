module Regla (
	Regla(..),
	analizaXML,
	evalua,
	evaluaIO,
	atomoConclusion,
	dispara
)where
	import ParteRegla
	import Operador
	import Bool
	import Atomo
	import qualified Data.Boolean as DB
	import MemoriaTrabajo

	data Regla t = Regla {
		condicion  :: [ParteRegla t],
		conclusion :: [ParteRegla t],
		marca      :: Bool,
		objetivo   :: Bool,
		disparo    :: Bool
	}

	instance (Show t, DB.Boolean t) => Show (Regla t) where
		show (Regla condicion conclusion _ _ _) = "SI "++cad_condicion++" "++"ENTONCES "++cad_conclusion where
			cad_condicion = unwords $ map descripcion condicion
			cad_conclusion = unwords $ map descripcion conclusion
			descripcion (PRA (Atomo d _ obj)) = if obj then '*':d else d
			descripcion (PRO o) = show o


	analizaXML ::(DB.Boolean t) => String -> (Regla t)
	analizaXML s = analizaXML' (words s) [] [] False False False False False False

	analizaXML' :: (DB.Boolean t) => [String] ->  [ParteRegla t] -> [ParteRegla t] ->Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Regla t
	analizaXML' [] condicion conclusion _ _ _ _ _ reglaObj = Regla (reverse condicion) (reverse conclusion) False False reglaObj
	analizaXML' (parte:resto) condicion conclusion regla cond conc atomo obj reglaObj = case parte of 
		"<atomo>"      -> analizaXML' resto condicion conclusion regla cond conc True False reglaObj
		"</atomo>"     -> analizaXML' resto condicion conclusion regla cond conc False False reglaObj
		"<atomoObj>"   -> analizaXML' resto condicion conclusion regla cond conc True True True
		"</atomoObj>"  -> analizaXML' resto condicion conclusion regla cond conc False False reglaObj
		"<condicion>"  -> analizaXML' resto condicion conclusion regla True conc atomo obj reglaObj
		"</condicion>" -> analizaXML' resto condicion conclusion regla False conc atomo obj reglaObj
		"<conclusion>" -> analizaXML' resto condicion conclusion regla cond True atomo obj reglaObj
		"</conclusion>"-> analizaXML' resto condicion conclusion regla cond False atomo obj reglaObj
		"<negacion/>"  -> if (cond && (not conc)) then (analizaXML' resto ((PRO Negacion):condicion) conclusion regla cond conc atomo obj reglaObj) else  if (conc && not cond) then (analizaXML' resto condicion ((PRO Negacion):conclusion) regla cond conc atomo obj reglaObj) else (analizaXML' resto condicion conclusion regla cond conc atomo obj reglaObj) 
		"<conjuncion/>"-> if (cond && (not conc)) then (analizaXML' resto ((PRO Conjuncion):condicion) conclusion regla cond conc atomo obj reglaObj) else  if conc && not cond then (analizaXML' resto condicion ((PRO Conjuncion):conclusion) regla cond conc atomo obj reglaObj) else (analizaXML' resto condicion conclusion regla cond conc atomo obj reglaObj)
		"<disyuncion/>"-> if (cond && (not conc)) then (analizaXML' resto ((PRO Disyuncion):condicion) conclusion regla cond conc atomo obj reglaObj) else  if conc && not cond then (analizaXML' resto condicion ((PRO Disyuncion):conclusion) regla cond conc atomo obj reglaObj) else (analizaXML' resto condicion conclusion regla cond conc atomo obj reglaObj)
		otherwise      -> if (cond && (not conc) && atomo) then (analizaXML' resto ((PRA (Atomo parte DB.true obj)):condicion) conclusion regla cond conc atomo obj reglaObj) else  if conc && not cond then (analizaXML' resto condicion ((PRA (Atomo parte DB.true obj)):conclusion) regla cond conc atomo obj reglaObj) else (analizaXML' resto condicion conclusion regla cond conc atomo obj reglaObj)

	evalua :: (DB.Boolean t) => Regla t -> MemoriaTrabajo t -> Maybe t
	evalua (Regla cond conc _ _ _) mt = evalua' cond [] mt

	evalua' :: (DB.Boolean t) => [ParteRegla t] -> [t] -> MemoriaTrabajo t -> Maybe t
	evalua' [] [r] mt = Just r
	evalua' ((PRO Conjuncion):resto) (verdad1:verdad2:pila) mt = evalua' resto ((verdad1 DB.&&* verdad2):pila) mt
	evalua' ((PRO Disyuncion):resto) (verdad1:verdad2:pila) mt = evalua' resto ((verdad1 DB.||* verdad2):pila) mt
	evalua' ((PRO Negacion):resto) (verdad1:pila) mt = evalua' resto ((DB.notB verdad1):pila) mt
	evalua' ((PRA (Atomo atomo val _)):resto) pila mt = case (consultaValor mt atomo) of 
		(Just v) -> evalua' resto (v:pila) mt 
		Nothing -> Nothing

	evalua' _ _ _ = Nothing


	evaluaIO :: (DB.Boolean t) => Regla t -> MemoriaTrabajo t -> IO (MemoriaTrabajo t, Maybe t)
	evaluaIO (Regla cond conc _ _ _) mt = evaluaIO' cond [] mt

	evaluaIO' :: (DB.Boolean t) => [ParteRegla t] -> [t] -> MemoriaTrabajo t -> IO (MemoriaTrabajo t, Maybe t)
	evaluaIO' [] [r] mt = return (mt,(Just r))
	evaluaIO' ((PRO Conjuncion):resto) (verdad1:verdad2:pila) mt = evaluaIO' resto ((verdad1 DB.&&* verdad2):pila) mt
	evaluaIO' ((PRO Disyuncion):resto) (verdad1:verdad2:pila) mt = evaluaIO' resto ((verdad1 DB.||* verdad2):pila) mt
	evaluaIO' ((PRO Negacion):resto) (verdad1:pila) mt = evaluaIO' resto ((DB.notB verdad1):pila) mt
	evaluaIO' ((PRA (Atomo atomo val _)):resto) pila mt = do
		(mt',v) <- consultaValorIO mt atomo 
		evaluaIO' resto (v:pila) mt' 
	evaluaIO' _ _ mt = return (mt,Nothing)

	atomoConclusion (Regla _ ((PRA atomo):_) _ _ _)  = atomo

	dispara (Regla _ conclusion _ _ _) mt = foldl 
		(\ mt (Atomo descripcion valor _) -> agrega mt descripcion valor ) mt $ extraeAtomosConclusion conclusion []

	extraeAtomosConclusion [] atomos = atomos
	extraeAtomosConclusion ((PRO Negacion):resto) ((Atomo descripcion valor obj ):atomos) = extraeAtomosConclusion resto ((Atomo descripcion (DB.notB valor ) obj):atomos) 
	extraeAtomosConclusion ((PRA atomo ):resto) atomos = extraeAtomosConclusion resto (atomo:atomos)
	extraeAtomosConclusion (_:resto) atomos = extraeAtomosConclusion resto atomos