
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ "basic example test" ~: (2 :: Int) ~=? 1 + 1,
        "another basic example" ~: True ~=? not False
      ]

testesTorres :: Test
testesTorres =
  TestLabel "Testes Torres" $
    test
      [ "validaPosicoesTorres 1" ~: (2 :: Int) ~=? True,
        "validaPosicoesTorres 2" ~: True ~=? not False
      ]

--mapa com caminho até à base
mapa1 :: Mapa
mapa1 =
 [ [t, t, r, a, a, a],
   [r, t, r, a, r, r],
   [r, t, r, a, r, t],
   [r, t, r, a, r, t],
   [r, t, t, t, t, t],
   [a, a, a, a, r, r]
 ]
 where
       t = Terra
       r = Relva
       a = Agua

--mapa sem caminho até à torre
mapa2 :: Mapa
mapa2 =
 [ [t, t, r, a, a, a],
   [r, t, r, a, r, r],
   [r, a, r, a, r, t],
   [r, t, r, a, r, t],
   [r, t, t, t, t, t],
   [a, a, a, a, r, r]
 ]
 where
       t = Terra
       r = Relva
       a = Agua

--torre válida
torre1 :: Torre
torre1 = Torre 
  {
    posicaoTorre = (1.5,5.5),
    danoTorre = 1,
    alcanceTorre = 3,
    rajadaTorre = 3,
    cicloTorre = 5,
    tempoTorre = 4,
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
  }

--torre que sobrepõe a torre 1 
torre2 :: Torre
torre2 = Torre 
  {
    posicaoTorre = (1.5,5.5),
    danoTorre = 2,
    alcanceTorre = 5,
    rajadaTorre = 7,
    cicloTorre = 1,
    tempoTorre = 9,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}
  }

torre3 :: Torre
torre3 = Torre {posicaoTorre = (0.5,2.5)}