module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String
showDeCaja (Bombilla True)  = "On" --"💡"
showDeCaja (Bombilla False) = "Off" --"⚪️"
showDeCaja (Nada)           = "Nada" --"🛑"

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
recCircuito ::
  (Caja -> a) ->
  (Circuito -> a -> Circuito -> a -> a) ->
  (Caja -> Circuito -> a -> Circuito -> a -> Caja -> a) ->
  Circuito -> a
recCircuito fCaja fSerie fParalelo = rec
  where
    rec (Caja c)                   = fCaja c
    rec (Serie cir1 cir2)          = fSerie cir1 (rec cir1) cir2 (rec cir2)
    rec (Paralelo c1 cir1 cir2 c2) = fParalelo c1 cir1 (rec cir1) cir2 (rec cir2) c2

-- 2: foldCircuito
foldCircuito ::
  (Caja -> a) ->
  (a -> a -> a) ->
  (Caja -> a -> a -> Caja -> a) ->
  Circuito -> a
foldCircuito fCaja fSerie fParalelo =
  recCircuito fCaja (\_ rec1 _ rec2 -> fSerie rec1 rec2) (\caja1 _ rec1 _ rec2 caja2 -> fParalelo caja1 rec1 rec2 caja2)

-- 3 invertido
invertido :: Circuito -> Circuito
invertido = foldCircuito Caja (flip Serie) (\c1 rec1 rec2 c2 -> Paralelo c2 rec2 rec1 c1)

-- 4: hayCaminoIluminado
hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado = foldCircuito isOn (&&) (\c1 rec1 rec2 c2 -> (isOn c1 && isOn c2) && (rec1 || rec2))

isOn:: Caja -> Bool
isOn = (== on)

-- 5: cantidadPrendidas
cantidadPrendidas:: Circuito -> Int
cantidadPrendidas = foldCircuito estado (+) (\c1 cir1 cir2 c2 -> estado c1 + cir1 + cir2 + estado c2)
  where estado = (\c -> if isOn c then 1 else 0)

-- 6: cajasDeCircuito
cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = foldCircuito (:[]) (++) (\c1 cir1 cir2 c2 -> [c1] ++ cir1 ++ cir2 ++ [c2])

-- 7: esCircuitoProlijo
esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito (const True) (\_ rec1 cir2 rec2 -> not (esSerie cir2) && rec1 && rec2) (\_ cir1 rec1 cir2 rec2 _ -> not (esSerieDesprolija cir1) && not (esSerieDesprolija cir2) && rec1 && rec2)
  where
    esSerieDesprolija (Serie _ cir2) = esSerie cir2
    esSerieDesprolija _              = False
    esSerie (Serie _ _) = True
    esSerie _           = False

-- 8: circuitoEmprolijado
circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado = undefined -- TODO: COMPLETAR

-- 9: tienenLaMismaEstructura
-- La idea aca es que usamos foldr para construir un monton de funciones que van tomando valores de el segundo circuito
tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura = foldCircuito fCaja fSerie fParalelo
  where
    -- si vaciamos el primer circuito se corre esto en el segundo
    fCaja _ cir2 = case cir2 of
      (Caja _) -> True
      _        -> False
    -- si primer circuito es serie -> matcheamos casos para el segundo
    fSerie rec1 rec2 cir2 = case cir2 of
      (Serie c1 c2) -> (rec1 c1) && (rec2 c2)
      _             -> False
    -- si primer circuito es paralelo -> matcheamos casos para el segundo
    fParalelo  _ rec1 rec2 _ cir2 = case cir2 of
      (Paralelo _ c1 c2 _) -> (rec1 c1) && (rec2 c2)
      _                    -> False

-- 10: subCircuitoMásResistente
subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = recCircuito
  Caja
  (\cir1 rec1 cir2 rec2 -> mejorSegun compararResistencia (Serie cir1 cir2 : rec1 : rec2 : []))
  (\caja1 cir1 rec1 cir2 rec2 caja2 -> mejorSegun compararResistencia (Paralelo caja1 cir1 cir2 caja2 : Caja caja1 : rec1 : rec2 : Caja caja2 : []))

resistenciaCircuito :: Circuito -> Float
resistenciaCircuito = foldCircuito resCaja (+) (\c1 rec1 rec2 c2 -> resCaja c1 + resCaja c2 + (1/rec1) + (1/rec2))
  where
    resCaja c = case c of
      on   -> 2
      off  -> 1
      Nada -> -2

compararResistencia :: Circuito -> Circuito -> Bool
compararResistencia = (\cir1 cir2 -> resistenciaCircuito cir1 > resistenciaCircuito cir2)

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y)

{-- 11: Definiciones

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
----------------------------------------

# Demostrar:

        alternado . alternado = id

  {C}   alternado ( alternado ) = id
  {EXT} ∀ c :: circuito, alternado ( alternado c ) = id c

  # procedemos por induccion sobre c

  P(c) : ∀ c :: circuito, alternado ( alternado c ) = id c

  # caso base

  P(Caja a) : alternado ( alternado Caja a ) = id (Caja a)

        alternado ( alternado Caja a ) =
   {AC} alternado ( Caja (cajaAlternada a) ) =
   {AC} Caja (cajaAlternada (cajaAlternada a)) =

  Esto podemos resolverlo facilmente sin agregar nada mas pero vamos a crear un lema nuevo para evitarnos escribir de mas en pasos subsiguientes:

   {LEMA 1} Caja (cajaAlternada (cajaAlternada a)) = id (Caja a)

    Por lema de generacion de cajas, la variable a solo puede ser Bombilla Bool o Nada:
    caso a = Nada)

          Caja (cajaAlternada (cajaAlternada Nada))) = id (Caja Nada)
    {CAN} Caja (cajaAlternada Nada) =
    {CAN} Caja Nada =
    {I}   Id (Caja Nada) ✓

    caso ∀x::bool. a = Bombilla x)

      Caja (cajaAlternada (cajaAlternada (Bombilla x))) = id (Caja (Bombilla x))

      Por lema de generacion de booleanos x es True o False:

    caso x = True)

          Caja (cajaAlternada (cajaAlternada (Bombilla True))) = id (Caja (Bombilla True))

    {CAB} Caja (cajaAlternada (Bombilla (not True))) =
    {NT}  Caja (cajaAlternada (Bombilla False)) =
    {CAB} Caja (Bombilla (not False)) =
    {NF}  Caja (Bombilla True) =
    {I}   Id (Caja (Bombilla True)) ✓

    caso x = False)

          Caja ( cajaAlternada (cajaAlternada (Bombilla False))) = id (Caja (Bombilla False))

    {CAB} Caja (cajaAlternada (Bombilla (not False))) =
    {NF}  Caja (cajaAlternada (Bombilla True)) =
    {CAB} Caja (Bombilla (not True)) =
    {NT} Caja (Bombilla False) =
    {I}   Id (Caja (Bombilla False)) ✓

    Luego, por lema de generacion de Cajas queda demostrado el Lema 1.

    Volvemos a P(Caja a): alternado ( alternado Caja a ) = id (Caja a)

    {AC} alternado ( Caja (cajaAlternada a) ) =
    {AC} Caja (cajaAlternada (cajaAlternada a)) =
    {LEMA 1} id (Caja a) ✓

  # Caso recursivo:

  ## Series
  quiero ver que ∀i, j :: Circuito. (P(i) ^ p(j)) -> P(Serie i j)

  P(Serie i j): alternado ( alternado (Serie i j) ) = id (Serie i j)

    {AS} alternado ( Serie (alternado i) (alternado j) ) =
    {AS} Serie (alternado ( alternado i )) ( alternado (alternado j) ) =
    {HI} Serie (id i) ( alternado (alternado j) ) =
    {HI} Serie (id i) (id j) =
    {I}  Serie i (id j) =
    {I}  Serie i j =
    {I}  Id (Serie i j) ✓

  ## Paralelo
  quiero ver que ∀i, j :: Circuito. ∀q, k:: Caja.
  (P(i)^P(j)^P(q)^P(k)) -> P(Paralelo q i j k)

  P(Paralelo q i j k): alternado (alternado (Paralelo q i j k )) = id ( Paralelo q i j k )

    {AP} alternado ( Paralelo (cajaAlternada q) (alternado i) (alternado j) (cajaAlternada k) ) =
    {AP} Paralelo (cajaAlternada  (cajaAlternada q)) (alternado alternado i) (alternado alternado j) (cajaAlternada (cajaAlternada k)) =
    {HI} Paralelo (cajaAlternada cajaAlternada q) (id i) (alternado alternado j) (cajaAlternada cajaAlternada k) =
    {HI} Paralelo (cajaAlternada cajaAlternada q) (id i) (id j) (cajaAlternada cajaAlternada k) =
    {I} Paralelo (cajaAlternada cajaAlternada q) i (id j) (cajaAlternada cajaAlternada k) =
    {i} Paralelo (cajaAlternada cajaAlternada q) i j (cajaAlternada cajaAlternada k) =
    {LEMA 1} Paralelo (id q) i j (cajaAlternada cajaAlternada k) =
    {LEMA 1} Paralelo (id q) i j (id k) =
    {I} Paralelo q i j (id k) =
    {I} Paralelo q i j k =
    {I} id (Paralelo q i j k) ✓

  ∴ Queda demostrada la propiedad alternado . alternado = id
--}
