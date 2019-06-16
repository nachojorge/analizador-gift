
module Etapa1 where 
import Data.Char (isSpace)
import Tipos


--- El programa leeMX lee una lista hasta encontrar una marca. Si no encuentra
--- marcas, devuelve Nothing.

leeMX :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
leeMX m [] = Nothing
leeMX m (x:xs)
  | m x       = Just ([],  x, xs)
  | otherwise = do (ws, z, zs) <- leeMX m xs
                   return (x:ws, z, zs)


--- El programa leeMO lee una lista hasta encontrar una marca. Si no encuentra
--- marcas, devuelve Nothing en el segundo argumento y la lista vacía en el
--- tercero.

leeMO :: (a -> Bool) -> [a] -> ([a], Maybe a, [a])
leeMO m [] = ([], Nothing, [])
leeMO m (x:xs)
  | m x = ([], Just x, xs)
  | otherwise = (x : ys, w, ws)
    where (ys, w, ws) = leeMO m xs


getCuestionario :: (CCuerpo a) => [ String ] -> Maybe (Cuestionario a)
getCuestionario txt =
  do (ys, zs) <- getEjercicios txt
     return ys


getEjercicio :: (CCuerpo a) => [ String ] -> Maybe (Ejercicio a, [ String ])
getEjercicio xs =
  do (c1, ys)  <- getComs   xs
     (nm ,ws)  <- getNombre ys
     (qas, zs) <- getCuerpo ws
     (c2 , tl) <- getComs   zs
     return (Ejercicio c1 nm qas c2, skipNLs tl)


--- Este analizador identifica una secuencia de Ejercicio. Para ello
--- debe usar el analizador anterior getEjercicio.
--- Identifica todos los ejercicios que pueda

-- getEjercicios :: (CCuerpo a) => [ String ] -> Maybe ([Ejercicio a], [ String ])
-- getEjercicios    []  = Just ([], [])
-- getEjercicios (x:xs) =
--   let Just (y, ys) = getEjercicios xs
--   in case getEjercicio (x:xs) of
--                       Nothing -> Just ([], x:xs)
--                       Just (ej, xs) -> Just (ej : y, ys)

getEjercicios :: (CCuerpo a) => [ String ] -> Maybe ([Ejercicio a], [ String ])
getEjercicios    []  = Just ([], [])
getEjercicios (x:xs) =
  case getEjercicio (skipNLs(x:xs)) of
                    Nothing       -> Just ([], x:xs)
                    Just (ej, ys) -> Just (ej : w, ws)
                      where Just (w, ws) = getEjercicios ys


--- Predicado; decide si una línea es un comentario
esComentario :: String -> Bool
esComentario xs = case xs of
                       '/':'/':_ -> True
                       xs        -> False


skipNLs :: [ String ] -> [ String ]
skipNLs = dropWhile (all isSpace)

--- Un comentario es una línea de la forma "//..."
--- getComs lee comentarios sucesivos. Devuelve Nothing
--- si no hay comentarios al inicio.
--- El inicio del comentario está dado por dos "//",
--- que se eliminan. Si hubieran más "/" al comienzo de la línea,
--- los "/" adicionales se reemplazan por blancos " ".

getComs :: [ String ] -> Maybe (Comentario, [ String ])
getComs [] = Nothing
getComs xs =
  let (cms, pnocm, qas) = leeMO (not . esComentario) xs
  in case (cms, pnocm) of
          ([], _) -> Nothing
          (cms, Nothing) -> Just (COM (quitarBarras cms), [])
          (cms, Just nocm) -> Just (COM (quitarBarras cms), nocm:qas)

quitarBarras :: [ String ] -> [ String ]
quitarBarras [] = []
quitarBarras (x:xs) = case x of
                          '/' : '/' : ys -> replaceBars ys : quitarBarras xs

replaceBars :: String -> String
replaceBars [] = []
replaceBars ('/' : xs) = ' ' : replaceBars xs
replaceBars xs = xs

--- Un nombre está parentizado por "::".
--- Debe empezar y terminar en una unica linea, y estar al comienzo.
--- getNombre lee un nombre separando la línea en dos. Devuelve Nothing
--- si no hay nombre al inicio.

getNombre ((':' : ':' : nmqas) : ys)
  = let (nm, qas) = break (== ':') nmqas
        f (w : ws) = if (all isSpace w) then ws else (w:ws)
    in case qas of
            _:':':zs  -> Just (nm, f (zs:ys))
            otherwise -> Nothing
getNombre _
  = Nothing 

instance CCuerpo Char where
  getCuerpo xs = do (qas, z, zs) <- leeMX esComentario xs
                    return (unlines qas, z:zs)


-- ["\n","// ejemplo de verdadero falso","//","::fv.1:: Grant murio en 1886 { FALSO }","// fin de pregunta","\n","// ejemplo de multiple opcion","::mo.2:: Los dos Reyes y los dos {=laberintos ~vasallos ~magos}","// fin de pregunta"]
