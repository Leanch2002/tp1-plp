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

recCircuito :: (Caja -> a)
            -> (Circuito -> a -> Circuito -> a -> a)
            -> (Caja -> Circuito -> a -> Circuito -> a -> Caja -> a)
            -> Circuito -> a
recCircuito fCaja fSerie fParalelo = rec
    where
    rec (Caja caja)                      = fCaja caja
    rec (Serie cir1 cir2)                = fSerie cir1 (rec cir1) cir2 (rec cir2)
    rec (Paralelo caja1 cir1 cir2 caja2) = fParalelo caja1 cir1 (rec cir1) cir2 (rec cir2) caja2

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
foldCircuito :: (Caja -> a)
             -> (a -> a -> a)
             -> (Caja -> a -> a -> Caja -> a)
             -> Circuito -> a
foldCircuito fCaja fSerie fParalelo = 
  recCircuito fCaja (\_ x _ y -> fSerie x y) (\caja1 _ x _ y caja2 -> fParalelo caja1 x y caja2)

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
