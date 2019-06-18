module Etapa3 where

import Tipos
import Data.Char (isSpace)

----------- mo2short

mo2short :: Cuestionario QA -> Cuestionario QA
mo2short [] = []
mo2short (x:xs) = (mo2shortEj x) : (mo2short xs)
                  where mo2shortEj (Ejercicio c1 nm qas c2) = (Ejercicio c1 nm (mo2shortQAS qas) c2)

mo2shortQAS :: [QA] -> [QA]
mo2shortQAS [] = []
mo2shortQAS (x:xs) = case x of
                          A ys -> case ys of
                                       MO ws -> A (mo2shortA ws) : mo2shortQAS xs
                                       ys    -> A ys : mo2shortQAS xs
                          x    -> x : mo2shortQAS xs

mo2shortA :: [Opcion] -> Respuesta
mo2shortA (x:xs)
  | allOK (x:xs) = SHORT (x:xs)
  | otherwise = MO (x:xs)

allOK :: [Opcion] -> Bool
allOK [] = True
allOK (x:xs) = case x of
                    OK ys  -> allOK xs
                    NOK ys -> False


----------- sortMO

sortMO :: Cuestionario QA -> Cuestionario QA
sortMO [] = []
sortMO (x:xs) = (sortMOEj x) : (sortMO xs)
                where sortMOEj (Ejercicio c1 nm qas c2) = (Ejercicio c1 nm (sortMOQAS qas) c2)

sortMOQAS :: [QA] -> [QA]
sortMOQAS [] = []
sortMOQAS (x:xs) = case x of
                        A ys -> case ys of
                                     MO ws -> A (MO (sortMOA ws [] [])) : sortMOQAS xs
                                     ys    -> A (ys) : sortMOQAS xs
                        x    -> x : sortMOQAS xs

sortMOA :: [Opcion] -> [Opcion] -> [Opcion] -> [Opcion]
sortMOA [] ys zs = (ys ++ zs)
sortMOA (x:xs) ys zs = case x of
                          OK ws -> sortMOA xs (ys ++ [x]) zs
                          NOK ws -> sortMOA xs ys (zs ++ [x])


----------- trim

trim :: Cuestionario QA -> Cuestionario QA
trim [] = []
trim (x:xs) = (trimEj x) : (trim xs)
              where trimEj (Ejercicio c1 nm qas c2) = (Ejercicio c1 nm (trimQAS qas) c2)

trimQAS :: [QA] -> [QA]
trimQAS [] = []
trimQAS (x:xs) = case x of
                   Q ys -> Q (limpiarEspaciosFragmentos ys) : trimQAS xs
                   A ys -> case ys of
                                MO ws    -> A (MO (limpiarEspaciosMO ws)) : trimQAS xs
                                SHORT ws -> A (SHORT (limpiarEspaciosMO ws)) : trimQAS xs
                                ys       -> A ys : trimQAS xs

limpiarEspaciosMO :: [Opcion] -> [Opcion]
limpiarEspaciosMO [] = []
limpiarEspaciosMO (x:xs) = case x of
                                OK ys  -> OK (limpiarEspaciosFragmentos ys) : limpiarEspaciosMO xs
                                NOK ys -> NOK (limpiarEspaciosFragmentos ys) : limpiarEspaciosMO xs

limpiarEspaciosFragmentos :: [Fragmento] -> [Fragmento]
limpiarEspaciosFragmentos [] = []
limpiarEspaciosFragmentos (x:xs) = case x of
                              TXT ys  -> TXT (dropSpacesFragmento ys) : limpiarEspaciosFragmentos xs
                              MATH ys -> MATH (dropSpacesFragmento ys) : limpiarEspaciosFragmentos xs
                              CODE ys -> CODE (dropSpacesFragmento ys) : limpiarEspaciosFragmentos xs

dropSpacesFragmento :: String -> String
dropSpacesFragmento [] = []
dropSpacesFragmento (x:xs)
  | isSpace x = dropSpacesFragmento xs
  | otherwise = dropFinalSpacesFragmento (x:xs)

dropFinalSpacesFragmento :: String -> String
dropFinalSpacesFragmento [] = []
dropFinalSpacesFragmento xxs@(x:xs)
  | isSpace (last xxs) = dropFinalSpacesFragmento (init xxs)
  | otherwise = xxs


----------- nodupMO

nodupMO   :: Cuestionario QA -> Cuestionario QA
nodupMO [] = []
nodupMO (x:xs) = (nodupMOEj x) : (nodupMO xs)
                 where nodupMOEj (Ejercicio c1 nm qas c2) = (Ejercicio c1 nm (nodupMOQAS qas) c2)

nodupMOQAS :: [QA] -> [QA]
nodupMOQAS [] = []
nodupMOQAS (x:xs) = case x of
                         A ys -> case ys of
                                      MO ws -> A (MO (nodupMOA ws)) : nodupMOQAS xs
                                      ys    -> A (ys) : nodupMOQAS xs
                         x    -> x : nodupMOQAS xs

nodupMOA :: [Opcion] -> [Opcion]
nodupMOA [] = []
nodupMOA xs
  | duplicada (last xs) (init xs) = nodupMOA (init xs)
  | otherwise                     = (nodupMOA (init xs)) ++ ([last xs])

duplicada :: Opcion -> [Opcion] -> Bool
duplicada _ [] = False
duplicada x (y:ys) = case (x,y) of
              (OK xs, OK ws)   -> case duplicadoF xs ws of 
                                      False -> duplicada x ys
                                      True  -> True
              (NOK xs, NOK ws) -> case duplicadoF xs ws of
                                      False -> duplicada x ys
                                      True  -> True
              (x,_)            -> duplicada x ys

duplicadoF :: [Fragmento] -> [Fragmento] -> Bool
duplicadoF [] [] = True
duplicadoF (x:xs) (y:ys) = case (x,y) of
              (TXT ws, TXT zs)   -> case duplicadoStr ws zs of
                                        False -> False
                                        True  -> duplicadoF xs ys
              (CODE ws, CODE zs) -> case ws == zs of
                                        True  -> duplicadoF xs ys
                                        False -> False
              (MATH ws, MATH zs) -> case ws == zs of
                                        True  -> duplicadoF xs ys
                                        False -> False
              (x, y)             -> False


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

duplicadoStr :: String -> String -> Bool
duplicadoStr x y = case ((split x),(split y))  of
              (w ,y) ->case w == y of
                    True -> True
                    False-> False

igualLista:: [String]->[String]->Bool
igualLista l1 l2 = l1 == l2

split :: String -> [String]
split "" = []
split xs = ys : (split . drop 1) zs
          where (ys, zs) = span (/=',') xs


----------- filtroFV

filtroFV :: Cuestionario QA -> Cuestionario QA
filtroFV xs = case getFVs xs of
                   []  -> xs
                   [y] -> xs
                   (ys) -> case esPobre ys of
                                  True -> removerFVs xs
                                  _    -> xs
                                  where esPobre ys
                                          | length (getFVTs ys) == 0           = True
                                          | length (getFVTs ys) == (length ys) = True
                                          | otherwise                          = False

removerFVs :: Cuestionario QA -> Cuestionario QA
removerFVs [] = []
removerFVs (x:xs) = case isFVEj x of
                         True -> removerFVs xs
                         False -> x:(removerFVs xs)

getFVTs :: Cuestionario QA -> [Ejercicio QA]
getFVTs xs = filter isFVTEj xs

getFVs :: Cuestionario QA -> [Ejercicio QA]
getFVs xs = filter isFVEj xs

isFVTEj :: Ejercicio QA -> Bool
isFVTEj (Ejercicio c1 nm qas c2) = isFVTQAS qas

isFVTQAS :: [QA] -> Bool
isFVTQAS [] = False
isFVTQAS (x:xs) = case x of
                       A ys -> case ys of
                                    FV True -> True
                                    _       -> isFVTQAS xs
                       x    -> isFVTQAS xs

isFVEj :: Ejercicio QA -> Bool
isFVEj (Ejercicio c1 nm qas c2) = isFVQAS qas

isFVQAS :: [QA] -> Bool
isFVQAS [] = False
isFVQAS (x:xs) = case x of
                   A ys -> case ys of
                                FV _ -> True
                                _    -> isFVQAS xs
                   x    -> isFVQAS xs


transformaciones = mo2short . sortMO . nodupMO . filtroFV . trim
