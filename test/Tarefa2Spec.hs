
module Tarefa2Spec (testesTarefa2) where
  
import Tarefa2 
import LI12425

import Test.HUnit

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [teste1, teste2, teste3, teste4, teste5, teste6, teste7]

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
  TestLabel "Testes para função ativaInimigo" $
   test 
    [
      "O portal está vazio" ~: (portalC, [inimigoA, inimigoB, inimigoC]) ~=? ativaInimigo portalC [inimigoA, inimigoB, inimigoC], 
      "O portal não esta vazio, mas a 1º onda não tem inimigos" ~: (portalB {ondasPortal = [onda2]}, [inimigoA, inimigoB, inimigoC]) 
                                                               ~=? ativaInimigo portalB [inimigoA, inimigoB, inimigoC],
      "O portal não está vazio, e a 1º onda tem inimigos"  ~: (portalD {ondasPortal = [onda3 {inimigosOnda = [inimigoC]}, onda1, onda2]}, [inimigoC ,inimigoA, inimigoB]) 
                                                          ~=? ativaInimigo portalD [inimigoA, inimigoB]
     
    ]

teste4 :: Test 
teste4 = 
  TestLabel "Testes para função terminouJogo" $
   test 
    [
      "O jogo não acabou (em progresso)" ~: False ~=? terminouJogo jogoA,
      "O jogo acabou, e o jogador perdeu. (Tem inimigos, mas a vida é negativa.)" ~: True ~=? terminouJogo jogoB,
      "O jogo acabou, e o jogador ganhou o jogo. (Não tem inimigos - portal inativo e vazio)" ~: True ~=? terminouJogo jogoC,
      "O jogo acabou, e o jogador perdeu o jogo. (Sem inimigos, mas base morta)" ~: True ~=? terminouJogo Jogo {baseJogo = baseB, 
             inimigosJogo = [], 
             portaisJogo = [portalA]}, 
      "Jogo em progresso, base com vida, inimigos inativos, mas sem inimigos em jogo" ~: False ~=? terminouJogo Jogo {baseJogo = baseA, 
             inimigosJogo = [], 
             portaisJogo = [portalA, portalB, portalC]}
    ]

teste5 :: Test 
teste5 =
  TestLabel "Teste para a função ganhouJogo" $
   test 
    [
      "O jogo acabou, e o jogador ganhou o jogo. (Não tem inimigos - portal inativo e vazio)" ~: True ~=? ganhouJogo jogoC, 
      "O jogo não acabou (em progresso)" ~: False ~=? ganhouJogo jogoA,
      "O jogo acabou, e o jogador perdeu. (Tem inimigos, mas a vida é negativa.)" ~: False ~=? ganhouJogo jogoB, 
      "O jogo acabou, e o jogador perdeu o jogo. (Sem inimigos, mas base morta)" ~: False ~=? ganhouJogo Jogo {baseJogo = baseB, 
             inimigosJogo = [], 
             portaisJogo = [portalA]}, 
      "Jogo em progresso, base com vida, inimigos inativos, mas sem inimigos em jogo" ~: False ~=? ganhouJogo Jogo {baseJogo = baseA, 
             inimigosJogo = [], 
             portaisJogo = [portalA, portalB, portalC]}
    ]

teste6 :: Test 
teste6 =
  TestLabel "Testes para a função perdeuJogo" $
   test 
    [
      "O jogo acabou, e o jogador perdeu. (Tem inimigos, mas a vida é negativa.)" ~: True ~=? perdeuJogo jogoB,
      "O jogo acabou, e o jogador perdeu o jogo. (Sem inimigos, mas base morta)" ~: True ~=? perdeuJogo Jogo {baseJogo = baseB, 
             inimigosJogo = [], 
             portaisJogo = [portalA]}, 
      "O jogo acabou, e o jogador ganhou o jogo. (Não tem inimigos - portal inativo e vazio)" ~: False ~=? perdeuJogo jogoC,
      "O jogo não acabou (em progresso)" ~: False ~=? perdeuJogo jogoA,
      "Jogo em progresso, base com vida, inimigos inativos, mas sem inimigos em jogo" ~: False ~=? perdeuJogo Jogo {baseJogo = baseA, 
             inimigosJogo = [], 
             portaisJogo = [portalA, portalB, portalC]}
    ]

teste7 :: Test 
teste7 =
  TestLabel "Testes para a função auxiliar verificaPortal" $
   test 
   [
    "Portal sem ondas" ~: True ~=? verificaPortal portalC,
    "Uma onda com inimigos" ~: False ~=? verificaPortal portalB,
    "Ondas vazias" ~: True ~=? verificaPortal portalA
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

-- Base com vida positiva.
baseA :: Base 
baseA = Base 
  { 
    vidaBase = 5,
    posicaoBase = (2.5, 4.5),
    creditosBase = 10
  } 

-- Base com nível de vida inferior a zero.
baseB :: Base 
baseB = Base 
  { 
    vidaBase = -5,
    posicaoBase = (2.5, 4.5),
    creditosBase = 10
  } 

-- Portal inativo. Ou seja, tem onda sem inimigos.
portalA :: Portal 
portalA = Portal 
  { 
    posicaoPortal = (0.5, 0.5),
    ondasPortal = [onda1] 
  }

-- Portal ativo. Ou seja, tem onda com inimigos 
portalB :: Portal 
portalB = Portal 
  { 
    posicaoPortal = (5.5, 2.5),
    ondasPortal = [onda1, onda2] 
  }

-- Portal vazio. Ou seja, não tem ondas
portalC :: Portal 
portalC = Portal 
  { 
    posicaoPortal = (5.5, 2.5),
    ondasPortal = [] 
  }
-- Portal ativo 
portalD :: Portal 
portalD = Portal 
 {
    posicaoPortal = (1.5, 0.5),
    ondasPortal = [onda3, onda1, onda2] 
 }

-- Torre com gelo
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

-- Torre com fogo
torreB :: Torre 
torreB = Torre {posicaoTorre = (0.5,2.5), 
                danoTorre = 5,
                alcanceTorre = 3,
                rajadaTorre = 3,
                cicloTorre = 5,
                tempoTorre = 4,
                projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
               }

-- Torre com resina
torreC :: Torre 
torreC = Torre {posicaoTorre = (5.5,5.5), 
                danoTorre = 8,
                alcanceTorre = 3,
                rajadaTorre = 3,
                cicloTorre = 5,
                tempoTorre = 4,
                projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 5}
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

-- Onda com inimigos 
onda2 :: Onda
onda2 = Onda 
 {
    inimigosOnda = [inimigoC],
    cicloOnda = 8,
    tempoOnda = 7,
    entradaOnda = 5
 }

-- Onda com inimigos
onda3 :: Onda
onda3 = Onda 
 {
    inimigosOnda = [inimigoC, inimigoC],
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
  projeteisInimigo = [projetilC]          
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
  projeteisInimigo = [projetilB]  
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
  projeteisInimigo = [projetilA]  
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
