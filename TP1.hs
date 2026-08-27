module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito

-- ATENCION!!! Descartar parte de esta descripcion, ya que esta hecha solo con el fin de que
-- lo entendamos nosotros. A la hora de entregar el TP final, recomiendo que sinteticemos lo importante.

-- Recursion estructural:
-- Primer arg: una funcion 'fCaja' que espera una Caja y devuelve un tipo 'a'
-- Segundo arg: una funcion 'fSerie' que espera Circuito, 'a', Circuito, 'a' y devuelve 'a'
-- Tercer arg: una funcion 'fParalelo' que espera Caja, Circuito, 'a', Circuito, 'a', Caja y devuelve 'a'
-- Cuarto arg: El circuito a procesar
-- Devuelve 'a' que es el resultado de haber procesado al circuito recursivamente
-- Como es recursion estructural, las funciones 'fSerie' y 'fParalelo' esperan, ademas del 'a', al mismo circuito sin procesar.
recCircuito :: (Caja -> a)
            -> (Circuito -> a -> Circuito -> a -> a)
            -> (Caja -> Circuito -> a -> Circuito -> a -> Caja -> a)
            -> Circuito -> a
recCircuito fCaja fSerie fParalelo = rec
    where
    rec (Caja caja)                      = fCaja caja
    rec (Serie cir1 cir2)                = fSerie cir1 (rec cir1) cir2 (rec cir2)
    rec (Paralelo caja1 cir1 cir2 caja2) = fParalelo caja1 cir1 (rec cir1) cir2 (rec cir2) caja2

-- Version con case of. (QUEDA A EVALUAR SU IMPLEMENTACION)
--recCircuito2 fCaja fSerie fParalelo c = case c of
--    Caja caja                      -> fCaja caja
--    Serie cir1 cir2                -> fSerie cir1 (rec cir1) cir2 (rec cir2)
--        where
--            rec = recCircuito2 fCaja fSerie fParalelo
--    Paralelo caja1 cir1 cir2 caja2 -> fParalelo caja1 cir1 (rec cir1) cir2 (rec cir2) caja2
--        where
--            rec = recCircuito2 fCaja fSerie fParalelo

-- Pruebas rapidas del 1)
miCircuito :: Circuito
miCircuito = 
  Serie
      ( Paralelo
          on
          (Paralelo off cajaNada cajaOn on)
          (Paralelo Nada cajaOn cajaOff Nada)
          on
      )
      cajaOn

-- 2: foldCircuito
-- ATENCION!!! Borrar parte de estos comentarios
-- Recursion estructural:
-- Al ser estructurar, las funciones 'g' y 'h' (descritas anteriormente en el ejercicio 1)
-- ahora tienen menos argumentos; ahora el circuito sin procesar es descartado, solo nos interesa el tipo 'a'.
-- Las funciones que recibe 'recCircuito' en nuestra declaracion hacen de "puente" a las funciones que recibe 'foldCircuito':
-- simplemente ignoramos lo que no nos sirve y le pasamos como argumento lo que necesitamos.
foldCircuito :: (Caja -> a)
             -> (a -> a -> a)
             -> (Caja -> a -> a -> Caja -> a)
             -> Circuito -> a
foldCircuito fCaja fSerie fParalelo = 
  recCircuito fCaja (\_ x _ y -> fSerie x y) (\caja1 _ x _ y caja2 -> fParalelo caja1 x y caja2)

-- Version explicita
-- foldCircuito f g h (Caja caja) = f caja
-- foldCircuito f g h (Serie cir1 cir2) = g (foldCircuito f g h cir1) (foldCircuito f g h cir2)
-- foldCircuito f g h (Paralelo caja1 cir1 cir2 caja2) = h caja1 (foldCircuito f g h cir1) (foldCircuito f g h cir2) caja2

-- 3 invertido
invertido :: Circuito -> Circuito
invertido = foldCircuito (\a -> Caja a) (\a b -> Serie b a) (\a b c d -> Paralelo d c b a)

-- 4: hayCaminoIluminado
hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado =  
  foldCircuito
   (\a -> valorCaja a) 
   (\rec1 rec2 -> rec1 || rec2)
   (\c1 rec1 rec2 c2 -> valorCaja c1 && valorCaja c2 && (rec1 || rec2))
  where valorCaja (Bombilla x) = x
{-  
    case cir of
    (Caja c) -> valorCaja c
    (Serie a b) -> hayCaminoIluminado a || hayCaminoIluminado b
    (Paralelo c1 a b c2) -> (valorCaja c1 && valorCaja c2 && (hayCaminoIluminado a || hayCaminoIluminado b))
  where valorCaja (Bombilla x) = x-}


-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas = undefined -- TODO: COMPLETAR

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = undefined -- TODO: COMPLETAR

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = undefined -- TODO: COMPLETAR

-- 8: circuitoEmprolijado

circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado = undefined -- TODO: COMPLETAR

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura = undefined -- TODO: COMPLETAR

-- 10: subCircuitoMásResistente

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = undefined -- TODO: COMPLETAR

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

-- TODO: COMPLETAR

--}
