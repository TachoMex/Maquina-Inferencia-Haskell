module MemoriaTrabajo(
	MemoriaTrabajo(..),
	agrega,
	consultaValor,
	consultaValorIO,
)where
	import qualified Data.Map as Map
	import qualified Data.Boolean as DB
	import Consultor
	data MemoriaTrabajo t = MemoriaTrabajo{
		atomos :: Map.Map String t
	}

	agrega :: MemoriaTrabajo t -> String -> t -> MemoriaTrabajo t
	agrega mt atomo valor = 
		if Map.member atomo mt' then
			error ("Atomo "++atomo++" duplicado")
		else 
			MemoriaTrabajo (Map.insert atomo valor mt')
		where
			mt' = atomos mt

	consultaValor :: MemoriaTrabajo t -> String -> Maybe t
	consultaValor mt atomo = Map.lookup  atomo mt'
		where mt' = atomos mt 

	consultaValorIO :: (DB.Boolean t) => MemoriaTrabajo t -> String ->IO (MemoriaTrabajo t, t)
	consultaValorIO mt atomo = do 
		let valor' = consultaValor mt atomo
		case valor' of 
			(Just v) -> return (mt, v)
			Nothing  -> do
				v <- preguntarAtomo atomo
				let mt' = agrega mt atomo v
				return (mt',v)

	instance (DB.Boolean t, Show t) => Show (MemoriaTrabajo t) where
		show (MemoriaTrabajo mt) = unlines $ map show $ Map.toList mt 



