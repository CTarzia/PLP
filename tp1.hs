import Data.List
import Test.HUnit

data Tarea =
  Basica String Int |
  Independientes Tarea Tarea |
  DependeDe Tarea Tarea Int deriving Eq

instance Show Tarea where
  show = foldTarea (\i h -> i)
    (\a b -> "(" ++ a ++ " y " ++ b ++ ")")
    (\a b h -> "(" ++ b ++ " tras " ++ a ++ ")")

-- Ejercicio 1

-- recTarea
recTarea :: (String -> Int -> a) ->
            (Tarea -> Tarea -> a -> a -> a) ->
            (Tarea -> Tarea -> a -> a -> Int -> a) -> Tarea -> a
recTarea fBasica fIndep fDep (Basica nombre tiempo) =
    fBasica nombre tiempo
recTarea fBasica fIndep fDep (Independientes tarea1 tarea2) =
    fIndep tarea1 tarea2 (seguir tarea1) (seguir tarea2)
        where seguir = recTarea fBasica fIndep fDep
recTarea fBasica fIndep fDep (DependeDe tarea1 tarea2 tiempo) = 
    fDep tarea1 tarea2 (seguir tarea1) (seguir tarea2) tiempo
        where seguir = recTarea fBasica fIndep fDep

-- foldTarea
foldTarea :: (String -> Int -> a) ->
             (a -> a -> a) ->
             (a -> a -> Int -> a) -> Tarea -> a
-- En fold no usamos los primeros dos argumentos de fIndep y fDep
foldTarea fBasica fIndep fDep =
    recTarea fBasica (\x y -> fIndep) (\x y -> fDep)

-- Ejercicio 2

-- cantidadDeTareasBasicas
cantidadDeTareasBasicas :: [Tarea] -> Int
cantidadDeTareasBasicas tareas =
    foldr (\tarea tareasBasicasDelResto -> tareasBasicasDelResto + contarEn tarea) 0 tareas
        where contarEn = foldTarea (\nombre tiempo -> 1)
                                   (+)
                                   (\t1 t2 h -> t1 + t2)

-- cantidadMaximaDeHoras
cantidadMaximaDeHoras :: [Tarea] -> Int
cantidadMaximaDeHoras tareas =
    foldr (\tarea rec -> rec + contarEn tarea) 0 tareas
        where contarEn = foldTarea (\nombre tiempo -> tiempo)
                                   (+)
                                   (\t1 t2 h -> t1 + t2 + h)

-- tareasMasLargas
tareasMasLargas :: Int -> [Tarea] -> [Tarea]
tareasMasLargas tiempo =
    filter (\tarea -> (cantidadMaximaDeHoras [tarea]) > tiempo)

-- Ejercicio 3

-- chauListas
chauListas :: [Tarea] -> Tarea
chauListas = foldr1 Independientes

-- Ejercicio 4

-- tareasBasicas
tareasBasicas :: Tarea -> [Tarea]
tareasBasicas = foldTarea (\nombre tiempo -> [Basica nombre tiempo])
                          (++)
                          (\tareas1 tareas2 h -> tareas1 ++ tareas2)

-- esSubTareaDe
esSubTareaDe :: String -> Tarea -> Bool
esSubTareaDe nombre tarea = nombre `elem` 
                                (map (\(Basica nombre tiempo) -> nombre)
                                     (tareasBasicas tarea))

-- tareasBasicasIniciales
tareasBasicasIniciales :: Tarea -> [Tarea]
tareasBasicasIniciales = foldTarea (\nombre tiempo -> [Basica nombre tiempo])
                                   (++)
                                   (\tareas1 tareas2 h -> tareas2)

-- tareasBasicasQueDependenDe
tareasBasicasQueDependenDe :: String -> Tarea -> [Tarea]
tareasBasicasQueDependenDe nombreTarea = recTarea (\nombre tiempo -> [])
                                             (\tarea1 tarea2 lista1 lista2 -> lista1 ++ lista2 )
                                             (\tarea1 tarea2 lista1 lista2 h -> 
                                                 if nombreTarea `esSubTareaDe` tarea2 
                                                    -- Las tareas basicas de tarea1 dependen todas,
                                                    -- y puede haber algunas de tarea2
                                                    then (tareasBasicas tarea1) ++ lista2
                                                    -- Como la tarea no es subtarea de tarea2
                                                    -- no hace falta ver lista2, siempre estara vacia
                                                    else lista1)



-- Ejercicio 5

-- cuelloDeBotella
cuelloDeBotella :: Tarea -> String
cuelloDeBotella tarea = nombreDe $ head (sortBy tieneMasQueDependen (tareasBasicas tarea))
                           where tieneMasQueDependen (Basica nombre1 _) (Basica nombre2 _) =
                                  -- Usamos flip ya que compare ordena de forma ascendente
                                  -- y queremos que el primero sea el que mas tareas que dependen tenga
                                  (flip compare) (length $ tareasBasicasQueDependenDe nombre1 tarea)
                                          (length $ tareasBasicasQueDependenDe nombre2 tarea)
                                 nombreDe (Basica nombre _) = nombre

-- Ejercicio 6

type LuzMagica a = (a -> a)

-- pasos
pasos :: Eq a => a -> [LuzMagica a] -> a -> Int
pasos zf = foldr (\luz frec ->
                     -- Devolvemos una funcion que toma un zi.
                     -- Si es igual a zf, devolvemos 0 (no hace falta usar luces magicas)
                     -- Sino, aplicamos la luz
                     (\z ->
                         if z == zf
                             then 0
                             else 1 + frec (luz z)))
                 (\z -> 0) -- Esto es porque la funcion lo requiere, no hay caso base porque es lista infinita

-- Tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

tarea1 = Basica "a" 3
tarea2 = Basica "b" 1
tarea3 = Basica "c" 1
tarea4 = Basica "d" 2
tarea5 = DependeDe (Independientes tarea2 tarea3) tarea4 2
tarea6 = Independientes tarea2 tarea1
tarea7 = DependeDe tarea6 tarea5 1
lista1 = [tarea1]
lista2 = [tarea2,tarea3,tarea4]
lista3 = [tarea1,tarea5]
lista4 = [tarea1,tarea2,tarea3,tarea4,tarea5,tarea6]
lista5 = [tarea2,tarea1]

sumas1 :: [LuzMagica Int]
sumas1 = ((+1):sumas1)
sumas123 :: [LuzMagica Int]
sumas123 = ((+1):((+2):((+3):sumas123)))

testsEj1 = test [
  "a" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1) (\t1 t2 s1 s2 h -> s1) tarea1,
  "b" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1) (\t1 t2 s1 s2 h -> s1) tarea6,
  "b" ~=? recTarea (\n h -> n) (\t1 t2 s1 s2 -> s1) (\t1 t2 s1 s2 h -> s1) tarea5,
  "a" ~=? foldTarea (\n h -> n) (\s1 s2 -> s1) (\s1 s2 h -> s1) tarea1,
  "b" ~=? foldTarea (\n h -> n) (\s1 s2 -> s1) (\s1 s2 h -> s1) tarea6,
  "b" ~=? foldTarea (\n h -> n) (\s1 s2 -> s1) (\s1 s2 h -> s1) tarea5
  ]

testsEj2 = test [
  1 ~=? cantidadDeTareasBasicas lista1,
  4 ~=? cantidadDeTareasBasicas lista3,
  9 ~=? cantidadDeTareasBasicas lista4,
  3 ~=? cantidadMaximaDeHoras lista1,
  9 ~=? cantidadMaximaDeHoras lista3,
  17 ~=? cantidadMaximaDeHoras lista4,
  [] ~=? tareasMasLargas 3 lista1,
  [tarea5] ~=? tareasMasLargas 3 lista3,
  [tarea5,tarea6] ~=? tareasMasLargas 3 lista4
  ]

testsEj3 = test [
  tarea1 ~=? chauListas lista1,
  tarea6 ~=? chauListas lista5,
  Independientes tarea2 (Independientes tarea3 tarea4) ~=? chauListas lista2
  ]

testsEj4 = test [
  lista1 ~=? tareasBasicas tarea1,
  lista2 ~=? tareasBasicas tarea5,
  [tarea2,tarea1] ~=? tareasBasicas tarea6,
  False ~=? esSubTareaDe "b" tarea1,
  True ~=? esSubTareaDe "b" tarea5,
  True ~=? esSubTareaDe "a" tarea6,
  [tarea1] ~=? tareasBasicasIniciales tarea1,
  [tarea4] ~=? tareasBasicasIniciales tarea5,
  [tarea2,tarea1] ~=? tareasBasicasIniciales tarea6,
  [] ~=? tareasBasicasQueDependenDe "b" tarea5,
  [tarea2,tarea3] ~=? tareasBasicasQueDependenDe "d" tarea5,
  [] ~=? tareasBasicasQueDependenDe "a" tarea6,
  [] ~=? tareasBasicasQueDependenDe "d" tarea6
  ]

testsEj5 = test [
  "a" ~=? cuelloDeBotella tarea1,
  "d" ~=? cuelloDeBotella tarea5,
  "d" ~=? cuelloDeBotella tarea7
  ]

testsEj6 = test [
  5 ~=? pasos 10 sumas1 5,
  30 ~=? pasos 60 sumas123 0
  ]