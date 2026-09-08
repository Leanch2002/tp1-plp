import           Test.HUnit
import           TP1

-- CIRCUITOS DE PRUEBA
miCircuito :: Circuito
miCircuito = Serie (Paralelo on (Paralelo off cajaNada cajaOn on) (Paralelo Nada cajaOn cajaOff Nada) on) cajaOn

miCircuitoInvertido :: Circuito
miCircuitoInvertido = Serie cajaOn (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada off) on)

-- En este circuito HAY un camino iluminado, y otros caminos que no
cirCaminoIluminado :: Circuito
cirCaminoIluminado = Serie (Paralelo on (Paralelo on cajaNada cajaOn on) (Paralelo Nada cajaOn cajaOff Nada) on) cajaOn

miCircuitoProlijo :: Circuito
miCircuitoProlijo = Serie (Serie cajaOn cajaOff) cajaOn
miCircuitoDesprolijo :: Circuito
miCircuitoDesprolijo = Serie cajaOn (Serie cajaOff cajaOn)
miCircuitoProlijoComplejo :: Circuito
miCircuitoProlijoComplejo = Paralelo on (Serie (Serie cajaOn cajaOn) cajaOff) cajaNada off
miCircuitoDesprolijoComplejo :: Circuito
miCircuitoDesprolijoComplejo = Paralelo on (Serie (Serie cajaOn (Serie cajaOff cajaNada)) cajaOff) cajaNada off

-- TESTS
testsInvertido :: Test
testsInvertido = TestList -- TODO: AGREGAR
  [ "Caja invertida (1)"
    ~: invertido cajaOn
    ~?= cajaOn,
    "Caja invertida (2)"
    ~: invertido cajaOff
    ~?= cajaOff, 
    "Caja invertida (3)"
    ~: invertido cajaNada
    ~?= cajaNada, 
    "Serie invertida (4)"
    ~: invertido (Serie cajaOn cajaOff)
    ~?= (Serie cajaOff cajaOn), 
    "Paralelo invertido (5)"
    ~: invertido (Paralelo on cajaOn cajaOff off)
    ~?= (Paralelo off cajaOff cajaOn on), 
    "Circuito invertido (6)"
    ~: invertido miCircuito
    ~?= miCircuitoInvertido,
    "Circuito invertido 2 veces es el mismo circuito (7)"
    ~: invertido (invertido miCircuito)
    ~?= id miCircuito
  ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList -- TODO: AGREGAR
  [ "En una caja con bombilla encendida hay camino iluminado (1)"
    ~: hayCaminoIluminado cajaOn
    ~?= True, 
    "Caja apagada (2)"
    ~: hayCaminoIluminado cajaOff
    ~?= False, 
    "Serie sin camino (3)"
    ~: hayCaminoIluminado (Serie cajaOn cajaOff)
    ~?= False, 
    "Serie con camino (4)"
    ~: hayCaminoIluminado (Serie cajaOn cajaOn)
    ~?= True, 
    "Paralelo sin camino por extremo (5)"
    ~: hayCaminoIluminado (Paralelo on cajaOn cajaOff off)
    ~?= False, 
    "Paralelo sin camino por medio (6)"
    ~: hayCaminoIluminado (Paralelo on cajaOff cajaOff on)
    ~?= False, 
    "Paralelo con camino (7)"
    ~: hayCaminoIluminado (Paralelo on cajaOn cajaOff on)
    ~?= True, 
    "Este circuito No tiene camino iluminado (8)"
    ~: hayCaminoIluminado miCircuito
    ~?= False, 
    "En este circuito HAY un camino iluminado, y otros caminos que no (9)"
    ~: hayCaminoIluminado cirCaminoIluminado
    ~?= True
  ]

testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList -- TODO: AGREGAR
  [ "Cantidad prendidas en caja prendida es 1 (1)"
    ~: cantidadPrendidas cajaOn
    ~?= 1,
    "Cantidad prendidas en caja apagada es 0 (2)"
    ~: cantidadPrendidas cajaOff
    ~?= 0,
    "Serie apagada (3)"
    ~: cantidadPrendidas (Serie cajaOff cajaOff)
    ~?= 0,
    "Serie con 1 caja prendida y una vacia (4)"
    ~: cantidadPrendidas (Serie cajaOn cajaNada)
    ~?= 1,
    "Paralelo apagado (5)"
    ~: cantidadPrendidas (Paralelo off cajaOff cajaOff off)
    ~?= 0,
    "Paralelo con 2 prendidas, 1 apagada, y 1 vacia (6)"
    ~: cantidadPrendidas (Paralelo on cajaOn cajaNada off)
    ~?= 2,
    "Circuito complejo con 6 cajas encendidas (7)"
    ~: cantidadPrendidas miCircuito
    ~?= 6,
    "Cambiar la estructura de un circuito no cambia la cantidad de luces prendidas (8)"
    ~: cantidadPrendidas miCircuito == cantidadPrendidas (invertido miCircuito)
    ~?= True
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList -- TODO: AGREGAR
  [ "La lista de cajas de un circuito con una única caja es la lista con esa caja (1)"
    ~: cajasDeCircuito cajaOn
    ~?= [on],
    "Serie (2)"
    ~: cajasDeCircuito (Serie cajaOn cajaNada)
    ~?= [on, Nada],
    "Paralelo (3)"
    ~: cajasDeCircuito (Paralelo on cajaOn cajaOff Nada)
    ~?= [on, on, off, Nada],
    "La lista de cajas del circuito de ejemplo (4)"
    ~: cajasDeCircuito miCircuito
    ~?= [on, off, Nada, on, on, Nada, on, off, Nada, on, on],
    "Al invertir el circuito tambien se invierte el orden de las cajas (5)"
    ~: cajasDeCircuito (invertido miCircuito)
    ~?= [on, on, Nada, off, on, Nada, on, on, Nada, off, on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList -- TODO: AGREGAR
  [ "Una caja es prolija (1)"
    ~: esCircuitoProlijo cajaOn
    ~?= True,
    "Una serie con dos circuitos en paralelo es un ciruito prolijo (2)"
    ~: esCircuitoProlijo miCircuito
    ~?= True,
    "Serie con 2 cajas (3)"
    ~: esCircuitoProlijo (Serie cajaOn cajaOff)
    ~?= True,
    "Serie con Serie a la izquierda (4)"
    ~: esCircuitoProlijo (Serie (Serie cajaOn cajaNada) cajaOff)
    ~?= True,
    "Serie con Serie a la derecha (5)"
    ~: esCircuitoProlijo (Serie cajaOff (Serie cajaOn cajaNada))
    ~?= False,
    "Paralelo sin anidar es prolijo (6)"
    ~: esCircuitoProlijo (Paralelo on cajaOn cajaNada off)
    ~?= True,
    "Una serie cuyo segundo circuito tambien es una serie es un ciruito desprolijo (7)"
    ~: esCircuitoProlijo miCircuitoDesprolijo
    ~?= False,
    "Un circuito complejo donde todas las series son prolijas (8)"
    ~: esCircuitoProlijo miCircuitoProlijoComplejo
    ~?= True,
    "Un circuito complejo donde una serie dentro de otra serie dentro de un paralelo tiene una serie como 2do circuito (9)"
    ~: esCircuitoProlijo miCircuitoDesprolijoComplejo
    ~?= False
  ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
  -- "show = showDeCircuitoConEstructura".
  -- De esa forma, podrán distinguir la estructura de los circuitos en serie.
--testsCircuitoEmprolijado :: Test
--testsCircuitoEmprolijado = TestList -- TODO: AGREGAR
--  [ "La versión emprolijada de una caja es la misma caja"
--    ~: circuitoEmprolijado cajaOn
--    ~?= cajaOn
--  ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList
  [ "Cajas con distinto contenido (1)"
    ~: tienenLaMismaEstructura cajaOn cajaNada
    ~?= True, 
    "Series con la misma estructura (2)"
    ~: tienenLaMismaEstructura
      (Serie (Serie cajaOn cajaOff) cajaOn)
      (Serie (Serie cajaNada cajaOn) cajaOff)
    ~?= True,
    "Series con distinta estructura (3)"
    ~: tienenLaMismaEstructura
      (Serie cajaOn cajaOff)
      (Serie (Serie cajaNada cajaOn) cajaOff)
    ~?= False,
    "Serie y paralelo (4)"
    ~: tienenLaMismaEstructura
        (Serie cajaOn cajaOff)
        (Paralelo on cajaOn cajaOff off)
    ~?= False,
    "Misma cantidad de cajas, y paralelos, diferente orden (5)"
    ~: tienenLaMismaEstructura
        (Paralelo on cajaOn (Paralelo off cajaNada cajaOn on) off)
        (Paralelo off  (Paralelo on cajaOn cajaNada off) cajaOn off)
    ~?= False
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente = TestList -- TODO: AGREGAR
  [

  ]

tests :: Test
tests = TestList
  [ TestLabel "invertido"                testsInvertido
  , TestLabel "hayCaminoIluminado"       testsHayCaminoIluminado
  , TestLabel "cantidadPrendidas"        testsCantidadPrendidas
  , TestLabel "cajasDeCircuito"          testsCajasDeCircuito
  , TestLabel "esCircuitoProlijo"        testsEsCircuitoProlijo
  --, TestLabel "circuitoEmprolijado"      testsCircuitoEmprolijado
  , TestLabel "tienenLaMismaEstructura"  testsTienenLaMismaEstructura
  , TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
  ]

main :: IO ()
main = runTestTT tests >>= print
