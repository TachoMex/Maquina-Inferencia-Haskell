import Regla
import ParteRegla
import Operador
import Atomo
import MemoriaTrabajo
import qualified Data.Map as Map
import ModuloConocimiento
import MotorInferencia

import Data.Boolean as DB
import System.Environment

--funcionPreguntador etiquetaPregunta mv atomo = do
--	set etiquetaPregunta [text := ("Se cumple "++atomo++"?")]
--	print "Going to Sleep"
--	respuesta <- takeMVar mv
--	print "Awake!"
--	case respuesta of 
--		"S" -> return DB.true
--		"N" -> return DB.false
--		otherwise -> funcionPreguntador etiquetaPregunta mv atomo

--obtenerResultados bc mt mvGUI etiq etiqAns= do
--	(mt',res) <- encandenarAdelanteIOf bc mt (funcionPreguntador etiq mvGUI)
--	set etiqAns [text := (show res)]
--	return ()

main = do
	(nombreBC:args) <- getArgs

	bc <- desdeArchivoXML nombreBC (nombreBC++".bc") :: IO (ModuloConocimiento Bool)
	putStrLn $ show bc
	let mt = MemoriaTrabajo $ Map.fromList [] :: MemoriaTrabajo Bool
	encandenarAdelanteIO bc mt True

--	start $ gui mt bc

--botonCmd mv textbox = do
--	ans <- get textbox text
--	putMVar mv ans
--	return ()

--gui mt bc = do

--	mvGui <- newEmptyMVar

--	f <- frame [text:= "Sistema Experto"]
--	respuesta 		<- textEntry f [enabled := True, wrap := WrapNone]
--	boton			<- button f [enabled := True, text:= "siguiente", on command :=  (botonCmd mvGui respuesta)]
--	etiquetaPregunta <- staticText f [text:= ""]
--	etiquetaRespuesta <- staticText f [text := ""]
--	set f [layout := fill $ container f $ margin 10 $ column 4 [widget etiquetaPregunta, widget respuesta, widget boton, widget etiquetaRespuesta],
--		clientSize := (sz 800 400)]
--	_ <- forkOS $ obtenerResultados bc mt mvGui etiquetaPregunta etiquetaRespuesta

--	return ()