
module Tarefa2Spec (testesTarefa2) where
  
import Tarefa2 
import LI12425

import Test.HUnit

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [teste1, teste2, teste3]

teste1 :: Test 
teste1 = 
  TestLabel "Testes para função inimigosNoAlcance" $
    test 
      [ "Não há inimigos" ~: []  ~=? inimigosNoAlcance torreB [],
        "Não há inimigos no alcance" ~: [] ~=? inimigosNoAlcance torreB [inimigoB],
        "Há algum inimigo no alcance" ~: [inimigoA] ~=? inimigosNoAlcance torreB [inimigoA, inimigoB]
      ] 

teste2 :: Test 
teste2 = 
  TestLabel "Testes para função danoInimigo" $
   test 
    [
      "Há dano" ~: inimigoA {vidaInimigo = 4} ~=? danoInimigo torreA inimigoA 
    
    ]

teste3 :: Test 
teste3 = 
  TestLabel "Testes para função terminouJogo" $
   test 
    [
      "O jogo não acabou (em progresso)" ~: False ~=? terminouJogo jogoA,
      "O jogo acabou, e o jogador perdeu" ~: True ~=? terminouJogo jogoB,
      "O jogo acabou, e o jogador ganhou o jogo" ~: True ~=? terminouJogo jogoC,
      "O jogo sem inimigos, mas base morta" ~: True ~=? terminouJogo Jogo {baseJogo = baseB, 
             inimigosJogo = [], 
             portaisJogo = [portalA]}
    
   
    
    ]



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

baseA :: Base 
baseA = Base 
  { 
    vidaBase = 5,
    posicaoBase = (2.5, 4.5),
    creditosBase = 10
  } 

-- base com nível de vida inferior a zero
baseB :: Base 
baseB = Base 
  { 
    vidaBase = -5,
    posicaoBase = (2.5, 4.5),
    creditosBase = 10
  } 

-- Portal inativo
portalA :: Portal 
portalA = Portal 
  { 
    posicaoPortal = (0.5, 0.5),
    ondasPortal = [onda1] 
  }

-- Portal ativo 
portalB :: Portal 
portalB = Portal 
  { 
    posicaoPortal = (5.5, 2.5),
    ondasPortal = [onda2, onda1] 
  }

-- Portal vazio
portalC :: Portal 
portalC = Portal 
  { 
    posicaoPortal = (5.5, 2.5),
    ondasPortal = [] 
  }

torreA :: Torre
torreA = Torre 
 {
    posicaoTorre = (5.5,1.5),
    danoTorre = 6,
    alcanceTorre = 5,
    rajadaTorre = 7,
    cicloTorre = 1,
    tempoTorre = 9,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}
 }

torreB :: Torre 
torreB = Torre {posicaoTorre = (0.5,2.5), 
                danoTorre = 5,
                alcanceTorre = 3,
                rajadaTorre = 3,
                cicloTorre = 5,
                tempoTorre = 4,
                projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 5}
               }

torreC :: Torre 
torreC = Torre {posicaoTorre = (5.5,5.5), 
                danoTorre = 8,
                alcanceTorre = 3,
                rajadaTorre = 3,
                cicloTorre = 5,
                tempoTorre = 4,
                projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
               }

-- Onda sem inimigos
onda1 :: Onda 
onda1 = Onda 
 {
    inimigosOnda = [],
    cicloOnda = 8,
    tempoOnda = 7,
    entradaOnda = 5
 }

-- onda com inimigos 
onda2 :: Onda
onda2 = Onda 
 {
    inimigosOnda = [inimigoC],
    cicloOnda = 8,
    tempoOnda = 7,
    entradaOnda = 5
 }

inimigoA :: Inimigo
inimigoA = Inimigo 
 { 
  posicaoInimigo = (1.5,1.5),
  direcaoInimigo = Sul, 
  vidaInimigo = 10,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = []          
 }

inimigoB :: Inimigo 
inimigoB = Inimigo 
 {
  posicaoInimigo = (5.5,4.5),
  direcaoInimigo = Este,
  vidaInimigo = 10, 
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = []  
 }

inimigoC :: Inimigo
inimigoC = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Este,
  vidaInimigo = 10, 
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = []  
 }

projetilA :: Projetil
projetilA = Projetil
  { tipoProjetil = Fogo,
    duracaoProjetil = Finita 5.0 
  }

projetilB :: Projetil
projetilB = Projetil
  { tipoProjetil = Gelo,
    duracaoProjetil = Infinita 
  }

projetilC :: Projetil 
projetilC = Projetil 
 {tipoProjetil = Resina, 
  duracaoProjetil = Infinita
 }

-- jogo em progresso
jogoA :: Jogo
jogoA = Jogo 
 {
  baseJogo = baseA,
  portaisJogo = [portalA, portalB],
  torresJogo = [torreA, torreB],
  mapaJogo = mapa1,
  inimigosJogo = [inimigoA, inimigoB], 
  lojaJogo = [(30, torreC)]
 }

-- Jogo da derrota
jogoB :: Jogo
jogoB = Jogo 
 {
  baseJogo = baseB,
  portaisJogo = [portalB],
  torresJogo = [torreA, torreB],
  mapaJogo = mapa1,
  inimigosJogo = [inimigoA, inimigoB], 
  lojaJogo = [(30, torreC)]
 }

-- Jogo da vitoria 
jogoC :: Jogo
jogoC = Jogo 
 {
  baseJogo = baseA,
  portaisJogo = [portalA, portalC],
  torresJogo = [torreA, torreB],
  mapaJogo = mapa1,
  inimigosJogo = [], 
  lojaJogo = [(30, torreC)]
 }
