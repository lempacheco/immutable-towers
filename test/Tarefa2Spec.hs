
module Tarefa2Spec (testesTarefa2) where
  
import Tarefa2 
import LI12425

import Test.HUnit

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8, teste9]

teste1 :: Test 
teste1 = 
  TestLabel "Testes para função inimigosNoAlcance" $
    test 
      [ "Não há inimigos" ~: []  ~=? inimigosNoAlcance torre3 [],
        "Não há inimigos no alcance" ~: [] ~=? inimigosNoAlcance torre3 [inimigo1],
        "Há algum inimigo no alcance" ~: [inimigo2] ~=? inimigosNoAlcance torre3 [inimigo2, inimigo1]
      ] 

teste2 :: Test 
teste2 = 
  TestLabel "Testes para função danoInimigo" $
   test 
    [
      "Há dano" ~: inimigo2 {vidaInimigo = 4} ~=? danoInimigo torre1 inimigo2,
      "Não há dano" ~: inimigo2 ~=? danoInimigo torre4 inimigo2
    ]

teste3 :: Test 
teste3 = 
  TestLabel "Testes para função ativaInimigo" $
   test 
    [
      "O portal está vazio" ~: (portal3, [inimigo2, inimigo1, inimigo3]) ~=? ativaInimigo portal3 [inimigo2, inimigo1, inimigo3], 
      "O portal não esta vazio, mas a 1º onda não tem inimigos" ~: (portal2 {ondasPortal = [onda2]}, [inimigo2, inimigo1, inimigo3]) 
                                                               ~=? ativaInimigo portal2 [inimigo2, inimigo1, inimigo3],
      "O portal não está vazio, e a 1º onda tem inimigos"  ~: (portal4 {ondasPortal = [onda3 {inimigosOnda = [inimigo3]}, onda1, onda2]}, [inimigo3 ,inimigo2, inimigo1]) 
                                                          ~=? ativaInimigo portal4 [inimigo2, inimigo1]
     
    ]

teste4 :: Test 
teste4 = 
  TestLabel "Testes para função terminouJogo" $
   test 
    [
      "O jogo não acabou (em progresso)" ~: False ~=? terminouJogo jogo1,
      "O jogo acabou, e o jogador perdeu. (Tem inimigos, mas a vida é negativa.)" ~: True ~=? terminouJogo jogo2,
      "O jogo acabou, e o jogador ganhou o jogo. (Não tem inimigos - portal inativo e vazio)" ~: True ~=? terminouJogo jogo3,
      "O jogo acabou, e o jogador perdeu o jogo. (Sem inimigos, mas base morta)" ~: True ~=? terminouJogo Jogo {baseJogo = base2, 
             inimigosJogo = [], 
             portaisJogo = [portal1]}, 
      "Jogo em progresso, base com vida, inimigos inativos, mas sem inimigos em jogo" ~: False ~=? terminouJogo Jogo {baseJogo = base1, 
             inimigosJogo = [], 
             portaisJogo = [portal1, portal2, portal3]}
    ]

teste5 :: Test 
teste5 =
  TestLabel "Teste para a função ganhouJogo" $
   test 
    [
      "O jogo acabou, e o jogador ganhou o jogo. (Não tem inimigos - portal inativo e vazio)" ~: True ~=? ganhouJogo jogo3, 
      "O jogo não acabou (em progresso)" ~: False ~=? ganhouJogo jogo1,
      "O jogo acabou, e o jogador perdeu. (Tem inimigos, mas a vida é negativa.)" ~: False ~=? ganhouJogo jogo2, 
      "O jogo acabou, e o jogador perdeu o jogo. (Sem inimigos, mas base morta)" ~: False ~=? ganhouJogo Jogo {baseJogo = base2, 
             inimigosJogo = [], 
             portaisJogo = [portal1]}, 
      "Jogo em progresso, base com vida, inimigos inativos, mas sem inimigos em jogo" ~: False ~=? ganhouJogo Jogo {baseJogo = base1, 
             inimigosJogo = [], 
             portaisJogo = [portal1, portal2, portal3]}
    ]

teste6 :: Test 
teste6 =
  TestLabel "Testes para a função perdeuJogo" $
   test 
    [
      "O jogo acabou, e o jogador perdeu. (Tem inimigos, mas a vida é negativa.)" ~: True ~=? perdeuJogo jogo2,
      "O jogo acabou, e o jogador perdeu o jogo. (Sem inimigos, mas base morta)" ~: True ~=? perdeuJogo Jogo {baseJogo = base2, 
             inimigosJogo = [], 
             portaisJogo = [portal1]}, 
      "O jogo acabou, e o jogador ganhou o jogo. (Não tem inimigos - portal inativo e vazio)" ~: False ~=? perdeuJogo jogo3,
      "O jogo não acabou (em progresso)" ~: False ~=? perdeuJogo jogo1,
      "Jogo em progresso, base com vida, inimigos inativos, mas sem inimigos em jogo" ~: False ~=? perdeuJogo Jogo {baseJogo = base1, 
             inimigosJogo = [], 
             portaisJogo = [portal1, portal2, portal3]}
    ]

teste7 :: Test 
teste7 =
  TestLabel "Testes para a função auxiliar verificaPortal" $
   test 
   [
    "Portal sem ondas" ~: True ~=? verificaPortal portal3,
    "Uma onda com inimigos" ~: False ~=? verificaPortal portal2,
    "Ondas vazias" ~: True ~=? verificaPortal portal1
   ]

teste8 :: Test 
teste8 =
  TestLabel "Testes para a função auxiliar getTiposProjsInimigo" $
   test 
   [
    "Inimigo sem projéteis" ~: [] ~=? getTiposProjsInimigo inimigo5,
    "Inimigo com projéteis" ~: [Fogo, Gelo, Resina] ~=? getTiposProjsInimigo inimigo4
   ]

teste9 :: Test
teste9 =
  TestLabel "Testes para a função auxiliar getTiposProjsInimigo" $
   test 
   [
    "Torre com Gelo e inimigo sem projéteis" ~: danoInimigo torre1 (inimigo5 {projeteisInimigo = [projetil1]}) ~=? atingeInimigo torre1 inimigo5,
    "Torre com Gelo e inimigo com Gelo" ~: danoInimigo torre1 (inimigo1 {projeteisInimigo = [projetil1 {duracaoProjetil = Finita 20.0}]}) ~=? atingeInimigo torre1 inimigo1,
    "Torre com Gelo e inimigo com Fogo" ~: danoInimigo torre1 (inimigo3 {projeteisInimigo = []}) ~=? atingeInimigo torre1 inimigo3,
    "Torre com Gelo e Inimigo com Resina" ~: danoInimigo torre1 (inimigo2 {projeteisInimigo =  [projetil1] ++ projeteisInimigo inimigo2}) 
    ~=? atingeInimigo torre1 inimigo2
   ]

mapa1 :: Mapa
mapa1 =
 [ [t,t,r,a,a,a,r,r,r,r,r,r,r,r,r,r],
   [r,t,r,a,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,t,r,a,r,t,r,r,r,r,r,r,r,r,r,r],
   [r,t,r,a,r,t,r,r,r,r,r,r,r,r,r,r],
   [r,t,t,t,t,t,r,r,r,r,r,r,r,r,r,r],
   [a,a,a,a,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,r,r,r,r,r,r,r,r,r,r,r,r,r,r,r]
 ]
 where
       t = Terra
       r = Relva
       a = Agua

-- Base com vida positiva.
base1 :: Base 
base1 = Base 
  { 
    vidaBase = 5,
    posicaoBase = (2, 4),
    creditosBase = 10
  } 

-- Base com nível de vida inferior a zero.
base2 :: Base 
base2 = Base 
  { 
    vidaBase = -5,
    posicaoBase = (2, 4),
    creditosBase = 10
  } 

-- Portal inativo. Ou seja, tem onda sem inimigos.
portal1 :: Portal 
portal1 = Portal 
  { 
    posicaoPortal = (0, 0),
    ondasPortal = [onda1] 
  }

-- Portal ativo. Ou seja, tem onda com inimigos 
portal2 :: Portal 
portal2 = Portal 
  { 
    posicaoPortal = (5, 2),
    ondasPortal = [onda1, onda2] 
  }

-- Portal vazio. Ou seja, não tem ondas
portal3 :: Portal 
portal3 = Portal 
  { 
    posicaoPortal = (5, 2),
    ondasPortal = [] 
  }
-- Portal ativo 
portal4 :: Portal 
portal4 = Portal 
 {
    posicaoPortal = (1, 0),
    ondasPortal = [onda3, onda1, onda2] 
 }

-- Torre com gelo
torre1 :: Torre
torre1 = Torre 
 {
    posicaoTorre = (5,1),
    danoTorre = 6,
    alcanceTorre = 5,
    rajadaTorre = 7,
    cicloTorre = 1,
    tempoTorre = 9,
    projetilTorre = projetil1,
    iteracoesDesdeInicioAnimacao = 1
 }

-- Torre com resina
torre2 :: Torre 
torre2 = Torre {posicaoTorre = (5,5), 
                danoTorre = 8,
                alcanceTorre = 3,
                rajadaTorre = 3,
                cicloTorre = 5,
                tempoTorre = 4,
                projetilTorre = projetil2,
                iteracoesDesdeInicioAnimacao = 1
               }

-- Torre com fogo
torre3 :: Torre 
torre3 = Torre {posicaoTorre = (0,2), 
                danoTorre = 5,
                alcanceTorre = 3,
                rajadaTorre = 3,
                cicloTorre = 5,
                tempoTorre = 4,
                projetilTorre = projetil3,
                iteracoesDesdeInicioAnimacao = 1
               }

-- Torre sem dano
torre4 :: Torre 
torre4 = Torre {posicaoTorre = (5,5), 
                danoTorre = 0,
                alcanceTorre = 3,
                rajadaTorre = 3,
                cicloTorre = 5,
                tempoTorre = 4,
                projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 5},
                iteracoesDesdeInicioAnimacao = 1
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
    inimigosOnda = [inimigo3],
    cicloOnda = 8,
    tempoOnda = 7,
    entradaOnda = 5
 }

-- Onda com inimigos
onda3 :: Onda
onda3 = Onda 
 {
    inimigosOnda = [inimigo3, inimigo3],
    cicloOnda = 8,
    tempoOnda = 7,
    entradaOnda = 5
 }

inimigo1 :: Inimigo 
inimigo1 = Inimigo 
 {
  posicaoInimigo = (5,4),
  direcaoInimigo = Este,
  vidaInimigo = 10, 
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [projetil1],
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (5,4),
  iteracoesDesdeInicioAnimacaoInimigo = 1      
 }

inimigo2 :: Inimigo
inimigo2 = Inimigo 
 { 
  posicaoInimigo = (1,1),
  direcaoInimigo = Sul, 
  vidaInimigo = 10,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [projetil2], 
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (1,1),
  iteracoesDesdeInicioAnimacaoInimigo = 1        
 }

inimigo3 :: Inimigo
inimigo3 = Inimigo 
 {
  posicaoInimigo = (0,0),
  direcaoInimigo = Este,
  vidaInimigo = 10, 
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [projetil3],
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (0,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1      
 }

-- inimigo com todos os tipos de projéteis
inimigo4 :: Inimigo
inimigo4 = Inimigo 
 {
  posicaoInimigo = (0,0),
  direcaoInimigo = Este,
  vidaInimigo = 10, 
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [projetil3, projetil1, projetil2],
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (0,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1      
 }

-- inimigo sem projéteis
inimigo5 :: Inimigo
inimigo5 = Inimigo 
 {
  posicaoInimigo = (0,0),
  direcaoInimigo = Este,
  vidaInimigo = 10, 
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [],
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (0,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1      
 }

projetil1 :: Projetil
projetil1 = Projetil
  { tipoProjetil = Gelo,
    duracaoProjetil = Finita 10.0 
  }

projetil2 :: Projetil 
projetil2 = Projetil 
 {tipoProjetil = Resina, 
  duracaoProjetil = Infinita
 }

projetil3 :: Projetil
projetil3 = Projetil
  { tipoProjetil = Fogo,
    duracaoProjetil = Finita 5.0 
  }

-- jogo em progresso
jogo1 :: Jogo
jogo1 = Jogo 
 {
  baseJogo = base1,
  portaisJogo = [portal1, portal2],
  torresJogo = [torre1, torre3],
  mapaJogo = mapa1,
  inimigosJogo = [inimigo2, inimigo1], 
  lojaJogo = [(30, torre2)]
 }

-- Jogo da derrota
jogo2 :: Jogo
jogo2 = Jogo 
 {
  baseJogo = base2,
  portaisJogo = [portal2],
  torresJogo = [torre1, torre3],
  mapaJogo = mapa1,
  inimigosJogo = [inimigo2, inimigo1], 
  lojaJogo = [(30, torre2)]
 }

-- Jogo da vitoria 
jogo3 :: Jogo
jogo3 = Jogo 
 {
  baseJogo = base1,
  portaisJogo = [portal1, portal3],
  torresJogo = [torre1, torre3],
  mapaJogo = mapa1,
  inimigosJogo = [], 
  lojaJogo = [(30, torre2)]
 }