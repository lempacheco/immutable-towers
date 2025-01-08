
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import LI12425
import Tarefa1 

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ testesFaux, testesPortais, testesInimigos, testesTorres, testesBase, testesExistePeloMenosUmCaminho] 

testesFaux :: Test 
testesFaux = 
  TestLabel "Testes funções auxiliares" $ 
   test 
     [ "eTerra 1" ~: True ~=? eTerra (0.5,0.5) mapa1, 
       "eTerra 2" ~: False ~=? eTerra (5.5,0.5) mapa1,

       "eRelva 1" ~: True ~=? eRelva (2.5,0.5) mapa1,
       "eRelva 2" ~: False ~=? eRelva (0.5,0.5) mapa1,

       "eAgua 1" ~: True ~=? eAgua (3.5,0.5) mapa1,
       "eAgua 2" ~: False ~=? eAgua (0.5,0.5) mapa1,

       "procuraTerreno - Terra" ~: True 
     ]

testesPortais :: Test 
testesPortais = 
  TestLabel "Testes portais" $ 
   test 
     [ "Há pelo menos um portal" ~: True ~=? peloMenosUmPortal [portalA, portalB],
       "Não há portais" ~: False ~=? peloMenosUmPortal [],

       "Estão todos posicionados sobre a terra" ~: True ~=? validaPosicaoPortal [portalA, portalC] mapa1,
       "Nem todos estão posicionados sobre a terra" ~: False ~=? validaPosicaoPortal [portalA, portalB, portalC] mapa1,
       "A lista de portais é vazia (sempre True)" ~: True ~=? validaPosicaoPortal [] mapa1,

       "A base não esta sobreposta sobre nenhum portal" ~: True ~=? naoSobrepostoBasePortal baseA [portalA, portalB, portalC], 
       "A base está sobreposta com algum portal" ~: False ~=? naoSobrepostoBasePortal baseB [portalA, portalB, portalC],
       "Não existe portais, lista vazia (sempre True)" ~: True ~=? naoSobrepostoBasePortal baseB [],

       "Não estão sobrepostos portais com torres" ~: True ~=? naoSobrepostoTorrePortal [torre1, torre2] [portalA,portalC],
       "Estão sobrepostos portais com torres" ~: False ~=? naoSobrepostoTorrePortal [torre1, torre2, torreA] [portalA, portalC],
       "Não existe torres, lista vazia (sempre True)" ~: True ~=? naoSobrepostoTorrePortal [] [portalA, portalC], 
       "Não existe portais, lista vazia (sempre True)" ~: True ~=? naoSobrepostoTorrePortal [torre1, torre2, torreA] []
     ]

testesInimigos :: Test 
testesInimigos = 
  TestLabel "Testes inimigos" $ 
   test 
     [ "O inimigo é válido" ~: True ~=? inimigosInicio [inimigoA, inimigoC, inimigoE] [portalA, portalC],
       "O inimigo não é válido, por causa da posiçao" ~: False ~=? inimigosInicio [inimigoA, inimigoB, inimigoC, inimigoE] [portalA, portalC], 
       "O inimigo não é válido, por causa da vida" ~: False ~=? inimigosInicio [inimigoA, inimigoC, inimigoD, inimigoE] [portalA, portalC], 
       "O inimigo não é válido por causa da lita de projéteis" ~: False ~=? inimigosInicio [inimigoA, inimigoC, inimigoE, inimigoF] [portalA, portalC], 

       "O inimigo está sobre Terra" ~: True ~=? inimigosTerra [inimigoA, inimigoC, inimigoE, inimigoG] mapa1, 
       "Nem todos os inimigos estão na terra (contém Relva)"  ~: False ~=? inimigosTerra [inimigoA, inimigoC, inimigoH, inimigoE, inimigoG] mapa1, 
       "Nem todos os inimigos estão na terra (contém Agua)" ~: False ~=? inimigosTerra [inimigoA, inimigoC, inimigoI, inimigoE, inimigoG] mapa1, 
       "Lista de inimigos vazia (sempre True)" ~: True ~=? inimigosTerra [] mapa1,

       "Não há sobreposição" ~: True ~=? inimigosTorre [inimigoA, inimigoC, inimigoG] [torre1, torreB],
       "Há sobreposição" ~: False ~=? inimigosTorre [inimigoA, inimigoC, inimigoG, inimigoH] [torre1, torreB],
       "Lista de inimigos vazia (sempre True)" ~: True ~=? inimigosTorre [] [torre1, torre2],
       "Lista de torres vazia (sempre True)" ~: True ~=? inimigosTorre [inimigoA, inimigoG] [],

       "A velocidade dos inimigos é positiva" ~: True ~=? velocidadeInimigos [inimigoA, inimigoG, inimigoE],
       "Nem todos os inimigos tem velocidade positiva" ~: False ~=? velocidadeInimigos [inimigoA, inimigoG, inimigoC, inimigoE],
       "A lista de inimigos é vazia (Sempre True)" ~: True ~=? velocidadeInimigos [],
       
       "A lista de projéteis está válida" ~: True ~=? normalizaInimigos [Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Resina}, 
                                                                                                      Projetil {tipoProjetil = Gelo}]},
                                                                         Inimigo {projeteisInimigo = []}],
       "Na lista de projeteis ativos existe mais do que um projetil do mesmo tipo" ~: False ~=? normalizaInimigos 
            [Inimigo {projeteisInimigo = [projetilA, 
                                          projetilA 
                                         ]}, 
             Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Resina}, 
                                          Projetil {tipoProjetil = Gelo}]}],
       "Na lista de projéteis ativos coexistem fogo e resina" ~: False ~=? normalizaInimigos 
            [Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Gelo}, 
                                          Projetil {tipoProjetil = Resina} 
                                         ]},
             Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Fogo}, 
                                          Projetil {tipoProjetil = Resina} 
                                         ]}],
       "Na lista de projéteis ativos coexistem fogo e gelo" ~: False ~=? normalizaInimigos 
            [Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Fogo}, 
                                          Projetil {tipoProjetil = Gelo} 
                                         ]}, 
             Inimigo {projeteisInimigo = [Projetil {tipoProjetil = Resina}, 
                                          Projetil {tipoProjetil = Gelo}]}]
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
        "naoSobrepostoTorres - teste com torres não válidas" ~:  False ~=? naoSobrepostoTorres [torre2, torre7],

        "validaTorre - torre válida" ~: True ~=? validaTorre jogo1,
        "validaTorre - torre não válida" ~: False ~=? validaTorre jogo2
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

        "naoSobrepostoBaseTorrePortal - teste com uma base válida" ~: True ~=? naoSobrepostoBaseTorrePortal base1 [torre1] [portal1],
        "naoSobrepostoBaseTorrePortal - teste com uma base não válida (sobreposta a uma torre)" ~: False ~=? naoSobrepostoBaseTorrePortal base4 [torre1] [portal1],
        "naoSobrepostoBaseTorrePortal - teste com uma base não válida (sobreposta a um portal)" ~: False ~=? naoSobrepostoBaseTorrePortal base5 [torre1] [portal1]
      ]

testesExistePeloMenosUmCaminho :: Test
testesExistePeloMenosUmCaminho = test [
    "existePeloMenosUmCaminho" ~: True ~=? existePeloMenosUmCaminho mapaInicial portalInicial baseInicial
  ]

mapaInicial :: Mapa 
mapaInicial = 
  [ [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,t,t,t,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,r,r,t,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,r,r,t,r,r,r,a,a,r,r,r],
    [r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,r,r,r,a,a,a,r,r,t],
    [t,t,t,r,r,r,t,r,r,a,a,a,r,t,t,t],
    [r,r,r,r,r,r,t,r,r,a,a,r,r,t,r,r],
    [r,r,r,r,r,r,t,t,t,t,t,t,t,t,t,t],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

portalInicial :: Portal
portalInicial = Portal {posicaoPortal = (0,9)}

baseInicial :: Base
baseInicial = Base {posicaoBase = (15,9)}

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
 [ [t, t, r, a, a, a, a, a, a, a, a],
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
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5},
    iteracoesDesdeInicioAnimacao = 1
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
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5},
    iteracoesDesdeInicioAnimacao = 1
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
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5},
    iteracoesDesdeInicioAnimacao = 1
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
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5},
    iteracoesDesdeInicioAnimacao = 1
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
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1},
    iteracoesDesdeInicioAnimacao = 1
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
    projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5},
    iteracoesDesdeInicioAnimacao = 1
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
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1},
    iteracoesDesdeInicioAnimacao = 1
  }

-- portal sobre a terra
portalA :: Portal 
portalA = Portal 
  { 
    posicaoPortal = (0.5, 0.5),
    ondasPortal = [] 
  }

-- portal não está sobre a terra 
portalB :: Portal 
portalB = Portal 
  { 
    posicaoPortal = (0.5, 1.5),
    ondasPortal = [] 
  }

-- portal sobre terra 
portalC :: Portal 
portalC = Portal 
  { 
    posicaoPortal = (5.5, 2.5),
    ondasPortal = [] 
  }

-- base não sobreposta com portal 
baseA :: Base 
baseA = Base 
  { 
    vidaBase = 5,
    posicaoBase = (5.5, 4.5),
    creditosBase = 10
  }

-- base sobreposta com um portal 
baseB :: Base 
baseB = Base 
  {
    vidaBase = 5,
    posicaoBase = (5.5, 2.5), 
     creditosBase = 10
  }
-- esta torre esta sobre a terra, e estará sobreposta com um portal 
torreA :: Torre
torreA = Torre 
 {
    posicaoTorre = (0.5,0.5),
    danoTorre = 2,
    alcanceTorre = 5,
    rajadaTorre = 7,
    cicloTorre = 1,
    tempoTorre = 9,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}, 
    iteracoesDesdeInicioAnimacao = 1
 }

torreB :: Torre 
torreB = Torre 
 {
    posicaoTorre = (2.5,0.5),
    danoTorre = 2,
    alcanceTorre = 5,
    rajadaTorre = 7,
    cicloTorre = 1,
    tempoTorre = 9,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}, 
    iteracoesDesdeInicioAnimacao = 1
 }


-- Este inimigo tem a posiçao inicial de um portal valido. Ou seja, é valido 
inimigoA :: Inimigo 
inimigoA = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (0.5,0.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1 
 }

-- Este inimigo não tem a posiçao inicial de um portal valido. Ou seja, é inválido
inimigoB :: Inimigo 
inimigoB = Inimigo 
 {
  posicaoInimigo = (1.5,1.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (1.5,1.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1 
 }

-- Este inimigo tem a vida positiva. Ou seja, é valido 
inimigoC :: Inimigo
inimigoC = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = -10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (0.5,0.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1 
 }

-- Este inimigo não tem a vida positiva. Ou seja, é inválido
inimigoD :: Inimigo 
inimigoD = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = -2.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [],
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (0.5,0.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1 
 }

-- Este inimigo tem a lista de projéteis ativos vazia. Ou seja, é valido 
inimigoE :: Inimigo 
inimigoE = Inimigo 
 {
  posicaoInimigo = (5.5,2.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (5.5,2.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1
 }

-- Este inimigo não tem a lista de projéteis ativos vazia. Ou seja, é inválido
inimigoF :: Inimigo 
inimigoF = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [projetilA, projetilB],
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (0.5,0.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1
 }

-- Este inimigo é válido, e está sobre a terra.
inimigoG :: Inimigo 
inimigoG = Inimigo 
 {
  posicaoInimigo = (1.5,3.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  tipoInimigo = MulherLanca,
  projeteisInimigo = [], 
  caminhoInimigo = [],
  acDirecao = (1.5,3.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1
 }

-- Esta sobre Relva
inimigoH :: Inimigo 
inimigoH = Inimigo 
 {
  posicaoInimigo = (2.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (2.5,0.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1
 }

-- Esta sobre Agua
inimigoI :: Inimigo 
inimigoI = Inimigo 
 {
  posicaoInimigo = (5.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca, 
  caminhoInimigo = [],
  acDirecao = (5.5,0.5),
  iteracoesDesdeInicioAnimacaoInimigo = 1 
 }

projetilA :: Projetil
projetilA = Projetil
  { tipoProjetil = Fogo,
    duracaoProjetil = Finita 5.0 
  }

projetilB :: Projetil
projetilB = Projetil
  { tipoProjetil = Gelo,
    duracaoProjetil = Finita 2.0
  }

projetilC :: Projetil 
projetilC = Projetil 
 {tipoProjetil = Resina, 
  duracaoProjetil = Infinita
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

--jogo válido
jogo1 :: Jogo
jogo1 = Jogo
  {
    baseJogo = base1,
    portaisJogo = [portal1],
    torresJogo = [torre1],
    mapaJogo = mapa1,
    inimigosJogo = [inimigoA],
    lojaJogo = [(100,torre1)]
  }

--jogo não válido - torre não válida
jogo2 :: Jogo
jogo2 = Jogo
  {
    baseJogo = base1,
    portaisJogo = [portal1],
    torresJogo = [torre3],
    mapaJogo = mapa1,
    inimigosJogo = [inimigoA],
    lojaJogo = [(100,torre1)]
  }