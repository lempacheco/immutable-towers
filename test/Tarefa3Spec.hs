
module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8, teste9, teste10, teste11, teste12, teste13, teste14, teste15, teste16]

-- detetarInimigos 
teste1 :: Test
teste1 =
  TestLabel "Testes para a função detetarInimigo" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? detetarInimigo torreA3 [],
        "Não tem inimigos no alcance" ~: [] ~=? detetarInimigo torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1.5,1.5)}], 
        "Tem inimigos no alcance" ~: [inimigoA3 {posicaoInimigo = (5.5,2.5)}, inimigoA3 {posicaoInimigo = (5.5,3.5)} ] ~=? detetarInimigo torreA3 [inimigoA3, 
                                                                                                                                                  inimigoA3 {posicaoInimigo = (5.5,2.5)}, 
                                                                                                                                                  inimigoA3 {posicaoInimigo = (5.5,3.5)}]
      ]

-- inimigosOrdenados  

teste2 :: Test
teste2 =
  TestLabel "Testes para a função inimigosOrdenados" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? inimigosOrdenados torreA3 [],
        "Não tem inimigos no alcance" ~: [inimigoA3 {posicaoInimigo = (1.5,1.5)}, inimigoA3] ~=? inimigosOrdenados torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1.5,1.5)}], 
        "Tem inimigos no alcance" ~: [inimigoA3 {posicaoInimigo = (5.5,2.5)}, inimigoA3 {posicaoInimigo = (5.5,3.5)}, inimigoA3] ~=? inimigosOrdenados torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5)}, 
                                                                                                                                                                inimigoA3 {posicaoInimigo = (5.5,2.5)}, 
                                                                                                                                                                inimigoA3]
      ]

-- tirosPossiveis 

teste3 :: Test
teste3 =
  TestLabel "Testes para a função tirosPossiveis" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: 0 ~=? tirosPossiveis torreA3 [],
        "Tem menos inimigos que a rajada da Torre" ~: 2 ~=? tirosPossiveis torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (5.5,2.5)}, inimigoA3 {posicaoInimigo = (5.5,3.5)}], 
        "Tem mais inimigos que a rajada da Torre" ~: 3 ~=? tirosPossiveis torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5)}, 
                                                                                  inimigoA3 {posicaoInimigo = (5.5,2.5)}, 
                                                                                  inimigoA3 {posicaoInimigo = (5.5,4.5)}, 
                                                                                  inimigoA3 {posicaoInimigo = (4.5,4.5)}]
      ]

-- inimigosSobreviventesAlcance

teste4 :: Test
teste4 =
  TestLabel "Testes para a função inimigosSobreviventesAlcance" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? inimigosSobreviventesAlcance torreA3 [],
        "Um inimigo morre" ~: [inimigoA3 {posicaoInimigo = (5.5,3.5), 
                                          vidaInimigo = 4.0, 
                                          projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                              inimigoA3] ~=? inimigosSobreviventesAlcance torreA3 [inimigoA3, 
                                                                                   inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 6.0}, 
                                                                                   inimigoA3{posicaoInimigo = (5.5,3.5), vidaInimigo = 10.0}], 
        "Tem inimigos sobreviventes" ~: [inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                         inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 10.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]},
                                         inimigoA3] 
                                     ~=? inimigosSobreviventesAlcance torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 16.0}, 
                                                                               inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 8}, 
                                                                               inimigoA3], 
        "Não tem inimigos sobreviventes" ~: [] ~=? inimigosSobreviventesAlcance torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 4.0}, 
                                                                                         inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0}], 
        "Só tem inimigos sobreviventes fora do alcance da torre" ~: [ inimigoA3 {posicaoInimigo = (1.5,1.5), vidaInimigo = 6.0},
                                                                      inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 6.0},
                                                                      inimigoA3 {posicaoInimigo = (0.5,0.5), vidaInimigo = 6.0}
                                                                    ] 
                                                  ~=? inimigosSobreviventesAlcance torreA3 [ inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 4.0},
                                                                                             inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0},
                                                                                             inimigoA3 {posicaoInimigo = (0.5,0.5), vidaInimigo = 6.0},
                                                                                             inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 6.0},
                                                                                             inimigoA3 {posicaoInimigo = (1.5,1.5), vidaInimigo = 6.0}
                                                                                            ]
      ]

-- dispaProjeteis 

teste5 :: Test
teste5 =
  TestLabel "Testes para a função disparaProjeteis" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: ([], torreA3) ~=? disparaProjeteis torreA3 [],
        "Não tem inimigos no alcance" ~: ([inimigoA3, inimigoA3 {posicaoInimigo = (1.5,1.5)}], torreA3) ~=? disparaProjeteis torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1.5,1.5)}], 
        "A torre não está pronta para disparar" ~: ([inimigoA3 {posicaoInimigo = (5.5,3.5)}, 
                                                     inimigoA3 {posicaoInimigo = (5.5,2.5)},
                                                     inimigoA3 {posicaoInimigo = (5.5,4.5)}, 
                                                     inimigoA3 {posicaoInimigo = (4.5,4.5)}], torreA3 {tempoTorre = 1}) ~=? disparaProjeteis torreA3 {tempoTorre = 2} [inimigoA3 {posicaoInimigo = (5.5,3.5)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (5.5,2.5)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (5.5,4.5)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (4.5,4.5)}], 
        "A torre está pronta para disparar, e dispara" ~: ([inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                                            inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 3.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]},  
                                                            inimigoA3 {posicaoInimigo = (4.5,3.5), vidaInimigo = 4.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                                            inimigoA3 {posicaoInimigo = (5.5,4.5), vidaInimigo = 8.0},
                                                            inimigoA3 
                                                           ],
                                                 torreA3 {tempoTorre = 2.0}) 
                                            ~=? disparaProjeteis torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 9.0}, 
                                                                          inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (5.5,4.5), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (4.5,3.5), vidaInimigo = 10.0}, inimigoA3], 
        "A torre está pronta para disparar, mas não dispara" ~: ([inimigoA3 {posicaoInimigo = (1.5,1.5), vidaInimigo = 9.0}, 
                                                                  inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 8.0}, 
                                                                  inimigoA3 {posicaoInimigo = (1.5,2.5), vidaInimigo = 8.0}, 
                                                                  inimigoA3],
                                                                torreA3 {tempoTorre = 0}) 
                                            ~=? disparaProjeteis torreA3 [inimigoA3 {posicaoInimigo = (1.5,1.5), vidaInimigo = 9.0}, 
                                                                          inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (1.5,2.5), vidaInimigo = 8.0}, 
                                                                          inimigoA3]
      ]

teste6 :: Test
teste6 =  
  TestLabel "Testes para a função lancaInimigo" $
    test
      [ "Onda não está ativa" ~: (portalA3 {ondasPortal = [ondaA'3, ondaB3]}, [inimigoA3]) ~=? lancaInimigo portalA3 [inimigoA3],
        "Onda ativa, com tempoOnda > 0" ~: (portalA3 {ondasPortal = [ondaB3 {tempoOnda = 1}, ondaA3]}, [inimigoA3]) ~=? 
                                            lancaInimigo portalA3 {ondasPortal = [ondaB3, ondaA3]} [inimigoA3],
        "Onda ativa, com tempoOnda == 0" ~: (portalA3 {ondasPortal = [ondaB3 {inimigosOnda = [], tempoOnda = 3}, ondaA3]}, [inimigoB3,inimigoA3]) ~=? 
                                            lancaInimigo portalA3 {ondasPortal = [ondaB3 {tempoOnda = 0}, ondaA3]} [inimigoA3],
        "Onda ativa, com tempoOnda < 0" ~: (portalA3 {ondasPortal = [ondaB3 {inimigosOnda = [], tempoOnda = 3}, ondaA3]}, [inimigoB3,inimigoA3]) ~=? 
                                            lancaInimigo portalA3 {ondasPortal = [ondaB3 {tempoOnda = -1}, ondaA3]} [inimigoA3], 
        "Primeira onda não tem inimigos" ~: (portalA3 {ondasPortal = [ondaA3]}, [inimigoA3]) ~=? 
                                            lancaInimigo portalA3 {ondasPortal = [ondaB3 {inimigosOnda = [],tempoOnda = -1}, ondaA3]} [inimigoA3]
       -- Não tem inimigos, a onda é retirada. 
      ]
       
teste7 :: Test
teste7 =
  TestLabel "Testes para a função atualizaInimigoGelo" $
    test
      [
        "Teste com um inimigo com velocidade nula" ~: inimigo1 ~=? atualizaInimigoGelo inimigo1,
        "Teste com um inimigo com velocidade não nula" ~: inimigo2 {velocidadeInimigo = 0} ~=? atualizaInimigoGelo inimigo2
      ]

teste8 :: Test
teste8 =
  TestLabel "Testes para a função atualizaInimigoResina" $
    test
      [
        "Teste com um inimigo com velocidade nula" ~: inimigo1 ~=? atualizaInimigoResina inimigo1,
        "Teste com um inimigo com velocidade não nula" ~: inimigo2 {velocidadeInimigo = 9.0} ~=? atualizaInimigoResina inimigo2
      ]

teste9 :: Test
teste9 =
  TestLabel "Teste para a função atualizaInimigoFogo" $
    test
      [
        "Teste com um inimigo não afetado por Fogo" ~: [inimigo2] ~=? atualizaInimigoFogo [inimigo2],
        "Teste com um inimigo afetado por Fogo" ~: [inimigo1 {vidaInimigo = 5.0}] ~=? atualizaInimigoFogo [inimigo1]
      ]

teste10 :: Test
teste10 = 
  TestLabel "Teste para a função inimigosSemVidaIs" $
    test
      [
        "Teste só com inimigos com vida" ~: [inimigo1,inimigo2] ~=? inimigosSemVidaIs [inimigo1,inimigo2],
        "Teste só com inimigos sem vida" ~: [] ~=? inimigosSemVidaIs [inimigo3,inimigo4],
        "Teste com inimigos com e sem vida" ~: [inimigo1] ~=? inimigosSemVidaIs [inimigo1,inimigo3]
      ]

teste11 :: Test
teste11 = 
  TestLabel "Teste para a função inimigosSemVidaB" $
    test
      [
        "Teste só com inimigos com vida" ~: base1 ~=? inimigosSemVidaB [inimigo1,inimigo2] base1,
        "Teste só com inimigos sem vida" ~: base1 {creditosBase = 25} ~=? inimigosSemVidaB [inimigo3,inimigo4] base1,
        "Teste com inimigos com e sem vida" ~: base1 {creditosBase = 15} ~=? inimigosSemVidaB [inimigo1,inimigo3] base1
      ]

teste12 :: Test
teste12 = 
  TestLabel "Teste para a função atualizaDistanciaPercorridaInimigos" $
    test
      [
        "Teste com inimigo com velocidade nula" ~: [inimigo1] ~=? atualizaDistanciaPercorridaInimigos 1 [inimigo1],
        "Teste com inimigo com velocidade não nula" ~: [inimigo2 {posicaoInimigo = (10.5,0.5)}] ~=? atualizaDistanciaPercorridaInimigos 1 [inimigo2]
      ]

teste13 :: Test
teste13 = 
  TestLabel "Teste para a função inimigoAtingeBaseIs" $
    test
      [
        "Teste só com inimigos com posições iguais à da base" ~: [] ~=? inimigoAtingeBaseIs base1 [inimigo1, inimigo3],
        "Teste só com inimigos com posições diferentes da base" ~: [inimigo2, inimigo4] ~=? inimigoAtingeBaseIs base1 [inimigo2, inimigo4],
        "Teste com inimigos com posições iguais e diferentes da base" ~: [inimigo2] ~=? inimigoAtingeBaseIs base1 [inimigo1, inimigo2]
      ]

teste14 :: Test
teste14 = 
  TestLabel "Teste para a função inimigoAtingeBaseB" $
    test
      [
        "Teste só com inimigos com posições iguais à da base" ~: base1 {vidaBase = 40.0} ~=? inimigoAtingeBaseB [inimigo1, inimigo3] base1,
        "Teste só com inimigos com posições diferentes da base" ~: base1 ~=? inimigoAtingeBaseB [inimigo2, inimigo4] base1,
        "Teste com inimigos com posições iguais e diferentes da base" ~: base1 {vidaBase = 45.0} ~=? inimigoAtingeBaseB [inimigo1, inimigo2] base1
      ]

teste15 :: Test 
teste15 = 
   TestLabel "Testes para a função disparaTodosProjeteis" $
    test
      [
        "Três torres, todas prontas para disparar" ~: ([inimigoA3 {posicaoInimigo = (1.5,4.5), vidaInimigo = 4, projeteisInimigo = [projetil2]}, 
                                                        inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 4, projeteisInimigo = [projetil2]}],[torreA3 {posicaoTorre = (0.5,1.5), tempoTorre = 2}, 
                                                                                                                                                  torreA3, 
                                                                                                                                                  torreA3 {posicaoTorre = (2.5,3.5), tempoTorre = 2}]) 
                                                    ~=? disparaTodosProjeteis [torreA3 {posicaoTorre = (0.5,1.5)}, 
                                                                               torreA3,
                                                                               torreA3 {posicaoTorre = (2.5,3.5)}]
                                                                              [inimigoA3, 
                                                                               inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 10}, 
                                                                               inimigoA3 {posicaoInimigo = (1.5,4.5), vidaInimigo = 10}, 
                                                                               inimigoA3 {posicaoInimigo = (4.5,4.5)}], 
         "O inimigo pertence ao alcance de mais de uma torre, simultaneamente" ~: ([inimigoA3 {posicaoInimigo = (1.5,4.5), vidaInimigo = 4, projeteisInimigo = [projetil2]}],[torreA3 {posicaoTorre = (0.5,1.5), tempoTorre = 2}, 
                                                                                                                                                                             torreA3, torreA3 {posicaoTorre = (2.5,1.5), tempoTorre = 2},
                                                                                                                                                                             torreA3 {posicaoTorre = (2.5,3.5), tempoTorre = 2}])
                                                                              ~=? disparaTodosProjeteis [torreA3 {posicaoTorre = (0.5,1.5)}, 
                                                                                                         torreA3,
                                                                                                         torreA3 {posicaoTorre = (2.5,1.5)},
                                                                                                         torreA3 {posicaoTorre = (2.5,3.5)}]
                                                                                                        [inimigoA3, 
                                                                                                         inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 10}, 
                                                                                                         inimigoA3 {posicaoInimigo = (1.5,4.5), vidaInimigo = 10}, 
                                                                                                         inimigoA3 {posicaoInimigo = (4.5,4.5)},
                                                                                                         inimigoA3 {posicaoInimigo = (1.5,1.5), vidaInimigo = 10}], 
        "Torre com diferentes tipos de Projéteis" ~: ([inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 4.0, projeteisInimigo = [projetil2]},
                                                       inimigoA3 {posicaoInimigo = (1.5,4.5), vidaInimigo = 4.0, projeteisInimigo = [projetil3]},
                                                       inimigoA3 {posicaoInimigo = (4.5,4.5), vidaInimigo = 2.0, projeteisInimigo = [projetil3]}],
                                                                    [torreA3 {tempoTorre = 1.0}, 
                                                                    torreB3 {tempoTorre = 2.0},
                                                                    torreA3 {posicaoTorre = (0.5,1.5), tempoTorre = 2.0} ]) 
                                                  ~=? disparaTodosProjeteis [torreA3 {tempoTorre = 2.0}, 
                                                                             torreB3, 
                                                                             torreA3 {posicaoTorre = (0.5,1.5)}] [inimigoA3, 
                                                                                                                  inimigoA3 {posicaoInimigo = (1.5,0.5), vidaInimigo = 10}, 
                                                                                                                  inimigoA3 {posicaoInimigo = (1.5,4.5), vidaInimigo = 10}, 
                                                                                                                  inimigoA3 {posicaoInimigo = (4.5,4.5)},
                                                                                                                  inimigoA3 {posicaoInimigo = (1.5,1.5), vidaInimigo = 10}]
      ]

teste16 :: Test 
teste16 = 
   TestLabel "Testes para a função lancaTodosInimigos" $
    test
     [
      "Sem portais e inimigos" ~:
      ([], []) ~=? lancaTodosPortais [] [],

      "Sem inimigos ativos" ~: ([portalA3 {ondasPortal = [ondaD'3, ondaA3]}, portalB3 {ondasPortal = [ondaC'3, ondaB3]}], [inimigoB3])
         ~=? lancaTodosPortais [portalA3 {ondasPortal = [ondaD3, ondaA3]}, portalB3 {ondasPortal = [ondaC3, ondaB3]}] [], 

      "Inimigos ja no mapa" ~: ([portalA3 {ondasPortal = [ondaD'3, ondaA3]}, 
                                 portalB3 {ondasPortal = [ondaC3 {tempoOnda = 3, entradaOnda = 0}, ondaB3]}], 
                                 [inimigoB3, inimigoA3, Inimigo {vidaInimigo = 2, 
                                                                 direcaoInimigo = Norte, 
                                                                 posicaoInimigo = (5.5, 2.5), 
                                                                 velocidadeInimigo = 10.0,
                                                                 ataqueInimigo = 5.0, 
                                                                 butimInimigo = 5, 
                                                                 projeteisInimigo = [], 
                                                                 tipoInimigo = MulherLanca}]) 
         ~=? lancaTodosPortais [portalA3 {ondasPortal = [ondaD3, ondaA3]}, 
                                portalB3 {ondasPortal = [ondaC3 {entradaOnda = 0}, ondaB3]}] 
                               [Inimigo {vidaInimigo = 2, 
                                         direcaoInimigo = Norte, 
                                         posicaoInimigo = (5.5, 2.5), 
                                         velocidadeInimigo = 10.0,
                                         ataqueInimigo = 5.0, 
                                         butimInimigo = 5, 
                                         projeteisInimigo = [], 
                                         tipoInimigo = MulherLanca}]

      
     ]

teste17 :: Test 
teste17 = 
   TestLabel "Testes para a função atualizaPortaisEInimigos" $
    test
     [
      "Recebe um jogo no estado inicial" ~: jogoInicial {portaisJogo = [portalA3{ondasPortal = [ondaA'3, ondaB3]}]}
                                         ~=? atualizaPortaisEInimigos jogoInicial, 
      "O jogo ja tem inimigos" ~: jogoInicial {inimigosJogo = [inimigoC3, inimigoD3, inimigoA3], portaisJogo = [portalA3{ondasPortal = [ondaB3]}]} 
                              ~=? atualizaPortaisEInimigos jogoInicial {inimigosJogo = [inimigoC3, inimigoD3], portaisJogo = [portalA3 {ondasPortal = [ondaA'3,ondaB3]}]}
     ]


mapa1 :: Mapa
mapa1 =
 [ [t, t, r, a, a, a],
   [r, t, r, a, r, r],                     
   [r, t, r, a, r, t],
   [r, t, r, a, t, t],
   [r, t, t, t, t, t],
   [a, a, a, a, r, r]
 ]
 where
       t = Terra
       r = Relva
       a = Agua


jogoInicial :: Jogo 
jogoInicial = Jogo 
 {
  baseJogo = base1 ,
  portaisJogo = [portalA3],
  torresJogo = [],
  mapaJogo = mapa1, 
  inimigosJogo = [], 
  lojaJogo = lojaA
 }
lojaA :: Loja
lojaA = [(50, torreA3)]

-- Torre com gelo
torreA3 :: Torre
torreA3 = Torre 
 {
    posicaoTorre = (5.5,1.5),
    danoTorre = 6,
    alcanceTorre = 3,
    rajadaTorre = 3,
    cicloTorre = 2,
    tempoTorre = 0,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}
 }

-- Torre com resina
torreB3 :: Torre 
torreB3 = Torre 
 {
    posicaoTorre = (2.5,3.5),
    danoTorre = 4,
    alcanceTorre = 5,
    rajadaTorre = 3,
    cicloTorre = 2,
    tempoTorre = 0,
    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}
 }


portalA3 :: Portal 
portalA3 = Portal 
 {
    posicaoPortal = (1.5, 0.5),
    ondasPortal = [ondaA3, ondaB3] 
 }

portalB3 :: Portal 
portalB3 = Portal 
 {
    posicaoPortal = (0.5, 0.5),
    ondasPortal = [ondaA3, ondaB3] 
 }

ondaA3 :: Onda
ondaA3 = Onda 
 {
    inimigosOnda = [inimigoA3],
    cicloOnda = 3,
    tempoOnda = 0,
    entradaOnda = 2
 }

ondaA'3 :: Onda
ondaA'3 = Onda 
 {
    inimigosOnda = [inimigoA3],
    cicloOnda = 3,
    tempoOnda = 0,
    entradaOnda = 1
 }

ondaB3 :: Onda
ondaB3 = Onda 
 {
    inimigosOnda = [inimigoB3],
    cicloOnda = 3,
    tempoOnda = 2,
    entradaOnda = 0
 }

ondaC3 :: Onda 
ondaC3 = Onda 
 {
   inimigosOnda = [inimigoA3, inimigoC3],
   cicloOnda = 3,
   tempoOnda = 0,
   entradaOnda = 2
 }

ondaC'3 :: Onda 
ondaC'3 = Onda 
 {
   inimigosOnda = [inimigoA3, inimigoC3],
   cicloOnda = 3,
   tempoOnda = 0,
   entradaOnda = 1
 }

ondaD3 :: Onda 
ondaD3 = Onda 
 {
   inimigosOnda = [inimigoB3, inimigoD3],
   cicloOnda = 3,
   tempoOnda = 0,
   entradaOnda = 0
 }

ondaD'3 :: Onda 
ondaD'3 = Onda 
 {
   inimigosOnda = [inimigoD3],
   cicloOnda = 3,
   tempoOnda = 3,
   entradaOnda = 0
 }
inimigoA3 :: Inimigo 
inimigoA3 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca
 }

inimigoB3 :: Inimigo 
inimigoB3 = Inimigo 
 {
  posicaoInimigo = (1.5, 0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca
 }

inimigoC3 :: Inimigo 
inimigoC3 = Inimigo 
 {
  posicaoInimigo = (0.5, 0.5),
  direcaoInimigo = Oeste,
  vidaInimigo = 6.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = [],
  tipoInimigo = MulherLanca
 }

inimigoD3 :: Inimigo 
inimigoD3 = Inimigo 
 {
  posicaoInimigo = (1.5, 0.5),
  direcaoInimigo = Este,
  vidaInimigo = 6.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca
 }



--projetil de tipo Fogo
projetil1 :: Projetil
projetil1 = Projetil
  { tipoProjetil = Fogo,
    duracaoProjetil = Finita 5.0 
  }

--projetil de tipo Gelo
projetil2 :: Projetil
projetil2 = Projetil
  { tipoProjetil = Gelo,
    duracaoProjetil = Finita 1.0
  }

--projetil de tipo Resina
projetil3 :: Projetil 
projetil3 = Projetil 
 {tipoProjetil = Resina, 
  duracaoProjetil = Infinita
 }

--inimigo com velocidade nula, afetado por todos os tipos de projetil e com posição igual à base1
inimigo1 :: Inimigo 
inimigo1 = Inimigo 
 {
  posicaoInimigo = (5.5,4.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 0.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [projetil1, projetil2, projetil3],
  tipoInimigo = MulherLanca
 }

--inimigo com velocidade não nula, não afetado por nenhum projetil e com posição diferente da base1
inimigo2 :: Inimigo 
inimigo2 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Este,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca 
 }

--inimigo com vida nula, butim 5 e com posição igual à base1
inimigo3 :: Inimigo 
inimigo3 = Inimigo 
 {
  posicaoInimigo = (5.5,4.5),
  direcaoInimigo = Sul,
  vidaInimigo = 0.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [],
  tipoInimigo = MulherLanca 
 }

--inimigo com vida nula, butim 10 e com posição diferente da base1
inimigo4 :: Inimigo 
inimigo4 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 0.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 10, 
  projeteisInimigo = [], 
  tipoInimigo = MulherLanca 
 }

base1 :: Base 
base1 = Base 
  { 
    vidaBase = 50.0,
    posicaoBase = (5.5, 4.5),
    creditosBase = 10
  }
