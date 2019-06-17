module Etapa2 where

import Data.Char (isSpace)
import Tipos
import Etapa1

--- El programa leeMOE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing en el segundo argumento y la lista vacía en el
--- tercero. Si el ESCAPE es el ultimo elemento, devuelve Nothing.

leeMOE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], Maybe a, [a])
leeMOE m e [] = Just ([], Nothing, [])
leeMOE m e [x]
  | e x       = Nothing
  | m x       = Just ([],  Just x, [])
  | otherwise = Just ([x], Nothing, [])
leeMOE m e (x:y:xs)
  | m x       = Just ([], Just x, y:xs)
  | otherwise = case (e x,e y) of
                     (True, True) -> do (ws, z, zs) <- leeMOE m e (y:xs)
                                        return (x:ws, z, zs)
                     (True, _)    -> do (ws, z, zs) <- leeMOE m e xs
                                        return (x:y:ws, z, zs)
                     (_, _)       -> do (ws, z, zs) <- leeMOE m e (y:xs)
                                        return (x:ws, z, zs)


--- El programa leeMXE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing. Si el ESCAPE es el ultimo elemento,
--- devuelve Nothing.

leeMXE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], a, [a])
leeMXE m e [] = Nothing
leeMXE m e [x]
  | m x       = Just ([], x, [])
  | otherwise = Nothing
leeMXE m e (x:y:xs)
  | m x       = Just ([], x, y:xs)
  | otherwise = case (e x, e y) of
                     (True, True) -> do (ws, z, zs) <- leeMXE m e (y:xs)
                                        return (x:ws, z, zs)
                     (True, _)    -> do (ws, z, zs) <- leeMXE m e xs
                                        return (x:y:ws, z, zs)
                     (_, _)       -> do (ws, z, zs) <- leeMXE m e (y:xs)
                                        return (x:ws, z, zs)

--- El programa readFV recibe un String, y consume del mismo
--- una palabra que representa un valor booleano de acuerdo
--- a la gramatica de GIFT*.

readFV :: String -> Maybe (Bool, String)
readFV str = case leeMX (== '}') str of
                  Nothing -> Nothing
                  Just (xs, y, ys)
                    | elem (dropSpaces xs) wTrue  -> Just (True, ys)
                    | elem (dropSpaces xs) wFalse -> Just (False, ys)
                    | otherwise -> Nothing
                    where wTrue  = [ "VERDADERO", "VERDAD", "V", "TRUE", "T" ]
                          wFalse = [ "FALSO", "FALSE", "F" ]

dropSpaces :: String -> String
dropSpaces [] = []
dropSpaces (x:xs)
  | isSpace x = dropSpaces xs
  | otherwise = dropFinalSpaces (x:xs)

dropFinalSpaces :: String -> String
dropFinalSpaces [] = []
dropFinalSpaces (x:xs)
  | isSpace x = ""
  | otherwise = (x:ws)
  where
    ws = dropFinalSpaces xs


getSeq :: ( a -> Maybe (b, a) ) -> a -> Maybe ([b], a)
getSeq f xs =
  case (f xs) of
      Nothing       -> Just ([], xs)
      Just (fx, zs) -> do (bs, a) <- getSeq f zs
                          return (fx:bs, a)


---- str2qas consume todo un string y lo convierte en [ QA ]
---- Devuelve Nothing si no se puede consumir toda la entrada.

str2qas :: String -> Maybe [ QA ]
str2qas xs = do (qs, zs) <- getSeq str2qa xs
                if (zs /= "") then Nothing else Just qs

---- Un QAs puede comenzar con una respuesta (marcada { ... }) o con
---- una pregunta (sin  marcas).


str2qa :: String -> Maybe (QA, String)
str2qa [] = Nothing
str2qa (x:xs)
      | x == '{'     = Just (A a, ax)
      | otherwise    = Just (Q q, qx)
      where Just (a, ax) = str2a xs
            Just (q, qx) = str2q (x:xs)


---- str2q procesa una Pregunta.

str2q :: String -> Maybe ( Pregunta  , String )
str2q = getSeq getFragmento

---- Analizador de Fragmento
---- No se aceptan caracteres especiales sin escape dentro de un fragmento.
---- Los mismos son: {, }, =, ~

getFragmento :: String -> Maybe (Fragmento, String)
getFragmento "" = Nothing
getFragmento (x:xs)
    | x `elem` ['{', '}', '=', '~'] =  Nothing
    | x == '$'                      = str2Math xs
    | x == '`'                      = str2Code xs
    | otherwise                     = str2Txt  (x:xs)

--- Los MATH contienen cualquier caracter con escape que no sea $
str2Math :: String -> Maybe ( Fragmento , String )
str2Math str = do (math, _ , zs) <- leeMXE marca escape str
                  return ( MATH math , zs )
         where marca x = x == '$'

--- Los CODE contienen cualquier caracter con escape que no sea `
str2Code :: String -> Maybe ( Fragmento , String )
str2Code str = do (code, _, zs) <- leeMXE marca escape str
                  return (CODE code, zs)
         where marca x = x == '`'

--- Los TXT en Q contienen cualquier caracter con escape que no sea ` $ {
str2Txt :: String -> Maybe ( Fragmento , String )
str2Txt str = do (code, z, zs) <- leeMOE marca escape str
                 case z of
                      Nothing -> return (TXT code, [])
                      Just y  -> return (TXT code, y:zs)
              where marca x = x `elem` ['`','$','{','}','~','=']


---- str2a procesa una respuesta.

str2a :: String -> Maybe ( Respuesta , String )
str2a "" = Nothing
str2a xxs@(x:xs)
  | isSpace x      = str2a xs
  | x == '='       = Just (MO z, cx)
  | x == '~'       = Just (MO z, cx)
  | x == '}'       = Just (ESSAY, xs)
  | otherwise      = do (b, ys) <- readFV xxs
                        return (FV b, ys)
  where Just (z,zx) = getSeq leerOpcion ws
        Just (ws, c, cx) = leeMXE (== '}') escape xxs

leerOpcion :: String -> Maybe ( Opcion , String )
leerOpcion [] = Nothing
leerOpcion ('}':xs) = Nothing
leerOpcion ('=':xs) = do (op, zs) <- getSeq getFragmento xs
                         return (OK op, zs)
leerOpcion ('~':xs) = do (op, zs) <- getSeq getFragmento xs
                         return (NOK op , zs)

--- Un QA puede abarcar varias líneas. Termina en un comentario.
--- Se devuelve una única línea.

instance CCuerpo QA where
   getCuerpo xs = do (ys, zs) <- getCuerpo xs
                     qas <- str2qas (ys::Cuerpo Char)
                     return (qas, zs)

--- Marca para detectar el escape

escape = (== '\\')
