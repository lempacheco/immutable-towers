
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import LI12425
import Tarefa1

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ 
        testesTorres,
        testesBase
      ]

testesTorres :: Test
testesTorres =
  TestLabel "Testes Torres" $
    test
      [ "validaPosicoesTorres - teste com torres válidas" ~: True ~=? validaPosicoesTorres [torre1, torre2] mapa1,
        "validaPosicoesTorres - teste com torres não válidas" ~: False ~=? validaPosicoesTorres [torre1, torre3] mapa1,

        "alcanceTorresPositivo - teste com torres válidas" ~:  True ~=? alcanceTorresPositivo [torre1, torre2],
        "alcanceTorresPositivo - teste com torres não válidas" ~: False ~=? alcanceTorresPositivo [torre1, torre4],

        "rajadaTorresPositivo - teste com torres válidas" ~: True ~=? rajadaTorresPositivo [torre1, torre2],
        "rajadaTorresPositivo - teste com torres não válidas" ~: False ~=? rajadaTorresPositivo [torre1, torre5],

        "cicloTorresNaoNegativo - teste com torres válidas" ~: True ~=? cicloTorresNaoNegativo [torre1, torre2],
        "cicloTorresNaoNegativo - teste com torres não válidas" ~: False ~=? cicloTorresNaoNegativo [torre1, torre6],

        "naoSobrepostoTorres - teste com torres válidas" ~: True ~=? naoSobrepostoTorres [torre1, torre2],
        "naoSobrepostoTorres - teste com torres não válidas" ~:  False ~=? naoSobrepostoTorres [torre2, torre7]
      ]

testesBase :: Test
testesBase =
  TestLabel "Testes Base" $
    test
      [
        "validaPosicaoBase - teste com uma base válida" ~: True ~=? validaPosicaoBase base1 mapa1,
        "validaPosicaoBase - teste com uma base não válida" ~: False ~=? validaPosicaoBase base2 mapa1,

        "creditoNaoNegativoBase - teste com uma base válida" ~: True ~=? creditoNaoNegativoBase base1,
        "creditoNaoNegativoBase - teste com uma base não válida" ~: False ~=? creditoNaoNegativoBase base3,

        "sobrepostoBaseTorrePortal - teste com uma base válida" ~: True ~=? sobrepostoBaseTorrePortal base1 [torre1] [portal1],
        "sobrepostoBaseTorrePortal - teste com uma base não válida (sobreposta a uma torre)" ~: False ~=? sobrepostoBaseTorrePortal base4 [torre1] [portal1],
        "sobrepostoBaseTorrePortal - teste com uma base não válida (sobreposta a um portal)" ~: False ~=? sobrepostoBaseTorrePortal base5 [torre1] [portal1]
      ]

portal1 = Portal {
  posicaoPortal = (0.5,0.5),
  ondasPortal = []
}

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

--torre válida (no mapa1)
torre1 :: Torre
torre1 = Torre 
  {
    posicaoTorre = (1.5,1.5),
    danoTorre = 1,
    alcanceTorre = 3,
    rajadaTorre = 3,
    cicloTorre = 5,
    tempoTorre = 4,
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
  }

--torre válida (no mapa1)
torre2 :: Torre
torre2 = Torre 
  {
    posicaoTorre = (1.5,0.5),
    danoTorre = 1,
    alcanceTorre = 3,
    rajadaTorre = 3,
    cicloTorre = 5,
    tempoTorre = 4,
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
  }

--torre posicionada sobre água no mapa1
torre3 :: Torre
torre3 = Torre 
  {
    posicaoTorre = (3.5,0.5),
    danoTorre = 1,
    alcanceTorre = 3,
    rajadaTorre = 3,
    cicloTorre = -1,
    tempoTorre = 4,
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
  }

--torre com o alcance negativo
torre4 :: Torre
torre4 = Torre 
  {
    posicaoTorre = (1.5,0.5),
    danoTorre = 1,
    alcanceTorre = -3,
    rajadaTorre = 3,
    cicloTorre = 9,
    tempoTorre = 4,
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
  }

--torre com a rajada nula
torre5 :: Torre
torre5 = Torre 
  {
    posicaoTorre = (1.5,0.5),
    danoTorre = 2,
    alcanceTorre = 5,
    rajadaTorre = 0,
    cicloTorre = 1,
    tempoTorre = 9,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}
  }

--torre com o ciclo negativo
torre6 :: Torre
torre6 = Torre 
  {
    posicaoTorre = (1.5,0.5),
    danoTorre = 1,
    alcanceTorre = 3,
    rajadaTorre = 3,
    cicloTorre = -1,
    tempoTorre = 4,
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
  }

--torre que sobrepõe a torre2 
torre7 :: Torre
torre7 = Torre 
  {
    posicaoTorre = (1.5,0.5),
    danoTorre = 2,
    alcanceTorre = 5,
    rajadaTorre = 7,
    cicloTorre = 1,
    tempoTorre = 9,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}
  }

--base válida (no mapa1)
base1 :: Base
base1 = Base
  {
    vidaBase = 3,
    posicaoBase = (5.5,4.5),
    creditosBase = 100
  }

--base posicionada sobre água no mapa1
base2 :: Base
base2 = Base
  {
    vidaBase = 3,
    posicaoBase = (5.5,0.5),
    creditosBase = 100
  }

--base com créditos negativos
base3 :: Base
base3 = Base
  {
    vidaBase = 3,
    posicaoBase = (5.5,4.5),
    creditosBase = -100
  }

--base sobreposta à torre1
base4 :: Base
base4 = Base
  {
    vidaBase = 3,
    posicaoBase = (1.5,1.5),
    creditosBase = 100
  }

--base sobreposta ao portal1
base5 :: Base
base5 = Base
  {
    vidaBase = 3,
    posicaoBase = (0.5,0.5),
    creditosBase = 100
  }