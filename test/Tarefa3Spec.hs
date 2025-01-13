module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3
import FuncoesAux

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ teste1, 
        teste2, 
        teste3, 
        teste4, 
        teste5, 
        teste6, 
        teste7, 
        teste8, 
        teste9, 
        teste10,
        teste11, 
        teste12, 
        teste13, 
        teste14, 
        teste15, 
        teste16,
        teste17, 
        teste18,
        teste19,
        teste20,
        teste21,
        teste22,
        teste23
      ]

-- detetarInimigos 
teste1 :: Test
teste1 =
  TestLabel "Testes para a função detetarInimigo" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? detetarInimigo torreA3 [],
        "Não tem inimigos no alcance" ~: [] ~=? detetarInimigo torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1,1)}], 
        "Tem inimigos no alcance" ~: [inimigoA3 {posicaoInimigo = (5,2)}, inimigoA3 {posicaoInimigo = (5,3)} ] ~=? detetarInimigo torreA3 [inimigoA3, 
                                                                                                                                                   inimigoA3 {posicaoInimigo = (5,2)}, 
                                                                                                                                                   inimigoA3 {posicaoInimigo = (5,3)}]
      ]

-- inimigosOrdenados
teste2 :: Test
teste2 =
  TestLabel "Testes para a função inimigosOrdenados" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? inimigosOrdenados torreA3 [],
        "Não tem inimigos no alcance" ~: [inimigoA3 {posicaoInimigo = (1,1)}, inimigoA3] ~=? inimigosOrdenados torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1,1)}], 
        "Tem inimigos no alcance" ~: [inimigoA3 {posicaoInimigo = (5,2)}, inimigoA3 {posicaoInimigo = (5,3)}, inimigoA3] ~=? inimigosOrdenados torreA3 [inimigoA3 {posicaoInimigo = (5,3)}, 
                                                                                                                                                                inimigoA3 {posicaoInimigo = (5,2)}, 
                                                                                                                                                                inimigoA3]
      ]

-- tirosPossiveis 

teste3 :: Test
teste3 =
  TestLabel "Testes para a função tirosPossiveis" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: 0 ~=? tirosPossiveis torreA3 [],
        "Tem menos inimigos que a rajada da Torre" ~: 2 ~=? tirosPossiveis torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (5,2)}, inimigoA3 {posicaoInimigo = (5,3)}], 
        "Tem mais inimigos que a rajada da Torre" ~: 3 ~=? tirosPossiveis torreA3 [inimigoA3 {posicaoInimigo = (5,3)}, 
                                                                                  inimigoA3 {posicaoInimigo = (5,2)}, 
                                                                                  inimigoA3 {posicaoInimigo = (5,4)}, 
                                                                                  inimigoA3 {posicaoInimigo = (4,4)}]
      ]

-- inimigosSobreviventes

teste4 :: Test
teste4 =
  TestLabel "Testes para a função inimigosSobreviventes" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? inimigosSobreviventes torreA3 [],
        "Um inimigo morre" ~: [inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 0.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]},
                              inimigoA3 {posicaoInimigo = (5,3), 
                                          vidaInimigo = 4.0, 
                                          projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                              inimigoA3] ~=? inimigosSobreviventes torreA3 [inimigoA3, 
                                                                                   inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 6.0}, 
                                                                                   inimigoA3{posicaoInimigo = (5,3), vidaInimigo = 10.0}], 
        "Tem inimigos sobreviventes" ~: [inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                         inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = 10.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]},
                                         inimigoA3] 
                                     ~=? inimigosSobreviventes torreA3 [inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = 16.0}, 
                                                                               inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 8}, 
                                                                               inimigoA3], 
        "Não tem inimigos sobreviventes" ~: [inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = -4.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.0}]},
                                             inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = -2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.0}]}] ~=? inimigosSobreviventes torreA3 [inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = 4.0}, 
                                                                                         inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 2.0}], 
        "Só tem inimigos sobreviventes fora do alcance da torre" ~: [ inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = -4.0,projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.0}]},
                                                                      inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = -2.0,projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.0}]},
                                                                      inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = 6.0},
                                                                      inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 6.0},
                                                                      inimigoA3 {posicaoInimigo = (0,0), vidaInimigo = 6.0}
                                                                    ] 
                                                  ~=? inimigosSobreviventes torreA3 [ inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = 4.0},
                                                                                             inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 2.0},
                                                                                             inimigoA3 {posicaoInimigo = (0,0), vidaInimigo = 6.0},
                                                                                             inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 6.0},
                                                                                             inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = 6.0}
                                                                                            ]
      ]

-- disparaProjeteis 

teste5 :: Test
teste5 =
  TestLabel "Testes para a função disparaProjeteis" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: ([], torreA3) ~=? disparaProjeteis torreA3 [],
        "Não tem inimigos no alcance" ~: ([inimigoA3, inimigoA3 {posicaoInimigo = (1,1)}], torreA3) ~=? disparaProjeteis torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1,1)}], 
        "A torre não está pronta para disparar" ~: ([inimigoA3 {posicaoInimigo = (5,3)}, 
                                                     inimigoA3 {posicaoInimigo = (5,2)},
                                                     inimigoA3 {posicaoInimigo = (5,4)}, 
                                                     inimigoA3 {posicaoInimigo = (4,4)}], torreA3 {tempoTorre = 1}) ~=? disparaProjeteis torreA3 {tempoTorre = 2} [inimigoA3 {posicaoInimigo = (5,3)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (5,2)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (5,4)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (4,4)}], 
        "A torre está pronta para disparar, e dispara" ~: ([inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                                            inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = 3.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]},  
                                                            inimigoA3 {posicaoInimigo = (4,3), vidaInimigo = 4.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                                            inimigoA3 {posicaoInimigo = (5,4), vidaInimigo = 8.0},
                                                            inimigoA3 
                                                           ],
                                                 torreA3 {tempoTorre = 2.0}) 
                                            ~=? disparaProjeteis torreA3 [inimigoA3 {posicaoInimigo = (5,3), vidaInimigo = 9.0}, 
                                                                          inimigoA3 {posicaoInimigo = (5,2), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (5,4), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (4,3), vidaInimigo = 10.0}, inimigoA3], 
        "A torre está pronta para disparar, mas não dispara" ~: ([inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = 9.0}, 
                                                                  inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 8.0}, 
                                                                  inimigoA3 {posicaoInimigo = (1,2), vidaInimigo = 8.0}, 
                                                                  inimigoA3],
                                                                torreA3 {tempoTorre = 0}) 
                                            ~=? disparaProjeteis torreA3 [inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = 9.0}, 
                                                                          inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (1,2), vidaInimigo = 8.0}, 
                                                                          inimigoA3]
      ]

-- lancaInimigo

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

--atualizaVelocidadeInimigoGeloEResina
teste7 :: Test
teste7 =
  TestLabel "Testes para a função atualizaVelocidadeInimigoGeloEResina" $
    test
      [
        "Teste com um inimigo com velocidade nula, afetado por Resina e Gelo" ~: [inimigo1 {projeteisInimigo = [projetil3, projetil2]}] ~=? atualizaVelocidadeInimigoGeloEResina [inimigo1{projeteisInimigo = [projetil3, projetil2]}],
        "Teste com um inimigo com velocidade não nula, afetado por Resina e Gelo" ~: [inimigo2 {velocidadeInimigo = 0, projeteisInimigo = [projetil3, projetil2]}] ~=? atualizaVelocidadeInimigoGeloEResina [inimigo2{projeteisInimigo = [projetil3,projetil2]}],
        "Teste com um inimigo com velocidade nula, afetado por Resina" ~: [inimigo1 {projeteisInimigo = [projetil3], velocidadeInimigo = 0.5}] ~=? atualizaVelocidadeInimigoGeloEResina [inimigo1{projeteisInimigo = [projetil3]}],
        "Teste com um inimigo com velocidade não nula, afetado por Resina" ~: [inimigo2 {projeteisInimigo = [projetil3], velocidadeInimigo = 0.5}]~=? atualizaVelocidadeInimigoGeloEResina [inimigo2{projeteisInimigo = [projetil3]}],
        "Teste com um inimigo com velocidade nula, afetado por Gelo" ~: [inimigo1{projeteisInimigo = [projetil2]}] ~=? atualizaVelocidadeInimigoGeloEResina [inimigo1{projeteisInimigo = [projetil2]}],
        "Teste com um inimigo com velocidade não nula, afetado por Gelo" ~: [inimigo2 {velocidadeInimigo = 0.0, projeteisInimigo = [projetil2]}]~=? atualizaVelocidadeInimigoGeloEResina [inimigo2{projeteisInimigo = [projetil2]}]
      ]

--atualizaInimigoFogo
teste8 :: Test
teste8 =
  TestLabel "Teste para a função atualizaInimigoFogo" $
    test
      [
        "Teste com um inimigo não afetado por Fogo" ~: [inimigo2] ~=? atualizaInimigoFogo [inimigo2],
        "Teste com um inimigo afetado por Fogo" ~: [inimigo1 {vidaInimigo = 10 - 5/60}] ~=? atualizaInimigoFogo [inimigo1]
      ]

--inimigosSemVida
teste9 :: Test
teste9 = 
  TestLabel "Teste para a função inimigosSemVida" $
    test
      [
        "Teste só com inimigos com vida" ~: (baseA, [inimigo1,inimigo2]) ~=? inimigosSemVida baseA [inimigo1,inimigo2],
        "Teste só com inimigos sem vida" ~: (baseA {creditosBase = 25},[]) ~=? inimigosSemVida baseA [inimigo3,inimigo4],
        "Teste com inimigos com e sem vida" ~: (baseA {creditosBase = 15} ,[inimigo1]) ~=? inimigosSemVida baseA [inimigo1,inimigo3]
      ]

--atualizaDistanciaPercorridaInimigos
teste10 :: Test
teste10 = 
  TestLabel "Teste para a função atualizaDistanciaPercorridaInimigos" $
    test
      [
        "Teste com inimigo com velocidade nula" ~: [inimigo1] ~=? atualizaDistanciaPercorridaInimigos 1 [inimigo1],
        "Teste com inimigo com velocidade não nula" ~: [inimigo2 {posicaoInimigo = (10,0)}] ~=? atualizaDistanciaPercorridaInimigos 1 [inimigo2]
      ]

--inimigoAtingeBase
teste11 :: Test
teste11 = 
  TestLabel "Teste para a função inimigoAtingeBase" $
    test
      [
        "Teste só com inimigos com posições iguais à da base" ~: (baseA {vidaBase = 40}, []) ~=? inimigoAtingeBase baseA [inimigo1, inimigo3],
        "Teste só com inimigos com posições diferentes da base" ~: (baseA, [inimigo2, inimigo4]) ~=? inimigoAtingeBase baseA [inimigo2, inimigo4],
        "Teste com inimigos com posições iguais e diferentes da base" ~: (baseA {vidaBase = 45},[inimigo2]) ~=? inimigoAtingeBase baseA [inimigo1, inimigo2]
      ]

-- disparaTodosProjeteis
teste12 :: Test 
teste12 = 
   TestLabel "Testes para a função disparaTodosProjeteis" $
    test
      [
        "Três torres, todas prontas para disparar" ~: ([inimigoA3 {posicaoInimigo = (1,4), vidaInimigo = 4, projeteisInimigo = [projetil2]}, 
                                                        inimigoA3 {posicaoInimigo = (4,4), vidaInimigo = 0, projeteisInimigo = [projetil2]},
                                                        inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 4, projeteisInimigo = [projetil2]},
                                                        inimigoA3 {posicaoInimigo = (0,0), vidaInimigo = 0, projeteisInimigo = [projetil2]}],[torreA3 {posicaoTorre = (0,1), tempoTorre = 2}, 
                                                                                                                                                  torreA3, 
                                                                                                                                                  torreA3 {posicaoTorre = (2,3), tempoTorre = 2}]) 
                                                    ~=? disparaTodosProjeteis [torreA3 {posicaoTorre = (0,1)}, 
                                                                               torreA3,
                                                                               torreA3 {posicaoTorre = (2,3)}]
                                                                              [inimigoA3, 
                                                                               inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 10}, 
                                                                               inimigoA3 {posicaoInimigo = (1,4), vidaInimigo = 10}, 
                                                                               inimigoA3 {posicaoInimigo = (4,4)}], 
         "O inimigo pertence ao alcance de mais de uma torre, simultaneamente" ~: ([inimigoA3 {posicaoInimigo = (1,4), vidaInimigo = 4, projeteisInimigo = [projetil2]},
                                                                                    inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = -8, projeteisInimigo = [projetil2 {duracaoProjetil = Finita 4}]},
                                                                                    inimigoA3 {posicaoInimigo = (4,4), vidaInimigo = 0, projeteisInimigo = [projetil2]},
                                                                                    inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = -2, projeteisInimigo = [projetil2 {duracaoProjetil = Finita 2}]},
                                                                                    inimigoA3 {posicaoInimigo = (0,0), vidaInimigo = -6, projeteisInimigo = [projetil2 {duracaoProjetil = Finita 2}]}],
                                                                              [torreA3 {posicaoTorre = (0,1), tempoTorre = 2}, 
                                                                               torreA3, torreA3 {posicaoTorre = (2,1), tempoTorre = 2},
                                                                               torreA3 {posicaoTorre = (2,3), tempoTorre = 2}])
                                                                              ~=? disparaTodosProjeteis [torreA3 {posicaoTorre = (0,1)}, 
                                                                                                         torreA3,
                                                                                                         torreA3 {posicaoTorre = (2,1)},
                                                                                                         torreA3 {posicaoTorre = (2,3)}]
                                                                                                        [inimigoA3, 
                                                                                                         inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 10}, 
                                                                                                         inimigoA3 {posicaoInimigo = (1,4), vidaInimigo = 10}, 
                                                                                                         inimigoA3 {posicaoInimigo = (4,4)},
                                                                                                         inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = 10}], 
        "Torre com diferentes tipos de Projéteis" ~: ([inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = 0.0, projeteisInimigo = [projetil2,projetil3]},
                                                       inimigoA3 {posicaoInimigo = (0,0), vidaInimigo = 0.0, projeteisInimigo = [projetil2]},
                                                       inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 4.0, projeteisInimigo = [projetil2]},
                                                       inimigoA3 {posicaoInimigo = (1,4), vidaInimigo = 6.0, projeteisInimigo = [projetil3]},
                                                       inimigoA3 {posicaoInimigo = (4,4), vidaInimigo = 2.0, projeteisInimigo = [projetil3]}],
                                                                    [torreA3 {tempoTorre = 1.0}, 
                                                                    torreB3 {tempoTorre = 2.0},
                                                                    torreA3 {posicaoTorre = (0,1), tempoTorre = 2.0} ]) 
                                                  ~=? disparaTodosProjeteis [torreA3 {tempoTorre = 2.0}, 
                                                                             torreB3, 
                                                                             torreA3 {posicaoTorre = (0,1)}] [inimigoA3, 
                                                                                                                  inimigoA3 {posicaoInimigo = (1,0), vidaInimigo = 10}, 
                                                                                                                  inimigoA3 {posicaoInimigo = (1,4), vidaInimigo = 10}, 
                                                                                                                  inimigoA3 {posicaoInimigo = (4,4)},
                                                                                                                  inimigoA3 {posicaoInimigo = (1,1), vidaInimigo = 10}]
      ]

-- lancaTodosPortais
teste13 :: Test 
teste13 = 
   TestLabel "Testes para a função lancaTodosPortais" $
    test
     [
      "Sem portais e inimigos" ~:
      ([], []) ~=? lancaTodosPortais [] [],

      "Sem inimigos ativos" ~: ([portalA3 {ondasPortal = [ondaD'3, ondaA3]}, portalB3 {ondasPortal = [ondaC'3, ondaB3]}], [inimigoB3])
         ~=? lancaTodosPortais [portalA3 {ondasPortal = [ondaD3, ondaA3]}, portalB3 {ondasPortal = [ondaC3, ondaB3]}] [], 

      "Inimigos ja no mapa" ~: ([portalA3 {ondasPortal = [ondaD'3, ondaA3]}], 
                                 [inimigoB3, inimigo2]) 
         ~=? lancaTodosPortais [portalA3 {ondasPortal = [ondaD3, ondaA3]}] 
                               [inimigo2]
     ]

-- atualizaPortaisEInimigos
teste14 :: Test 
teste14 = 
   TestLabel "Testes para a função atualizaPortaisEInimigos" $
    test
     [
      "Recebe um jogo no estado inicial" ~: jogoInicial {portaisJogo = [portalA3{ondasPortal = [ondaA'3, ondaB3]}]}
                                         ~=? atualizaPortaisEInimigos jogoInicial, 
      "O jogo ja tem inimigos" ~: jogoInicial {inimigosJogo = [inimigoC3, inimigoD3], portaisJogo = [portalA3{ondasPortal = [ondaA'3 {entradaOnda = 0}, ondaB3]}]} 
                              ~=? atualizaPortaisEInimigos jogoInicial {inimigosJogo = [inimigoC3, inimigoD3], portaisJogo = [portalA3 {ondasPortal = [ondaA'3,ondaB3]}]}, 
      "Os portais do jogo não estão prontos para lançar" ~: jogoInicial {inimigosJogo = [inimigoC3, inimigoD3], portaisJogo = [portalA3 {ondasPortal = [ondaD'3 {tempoOnda = 2},ondaC3]}]}
                                                           ~=? atualizaPortaisEInimigos jogoInicial {inimigosJogo = [inimigoC3, inimigoD3], portaisJogo = [portalA3 {ondasPortal = [ondaD'3,ondaC3]}]}
     ]

-- duraçaoFogoOuGelo
teste15 :: Test 
teste15 = 
   TestLabel "Testes para a função duracaoFogoOuGelo" $
    test
     [
      "Recebe uma lista de projéteis vazia" ~: [] ~=? duracaoFogoOuGelo [], 
      "Recebe uma lista de projéteis apenas de resina" ~: [projetil3] ~=? duracaoFogoOuGelo [projetil3],
      "Recebe uma lista com projéteis" ~: [projetil1 {duracaoProjetil = Finita 4.0}, projetil3] ~=? duracaoFogoOuGelo [projetil1, projetil2, projetil3]
     ]

-- atualizaDuracaoProjeteisInimigos
teste16 :: Test 
teste16 = 
  TestLabel "Testes para a função atualizaDuracaoProjeteisInimigos" $
   test
    [
     "Recebe um inimigo sem projeteis" ~: inimigoA3 ~=? atualizaDuracaoProjeteisInimigos inimigoA3, 
     "Recebe inimigo com projéteis" ~: inimigo1 {projeteisInimigo = [projetil1 {duracaoProjetil = Finita 4.0}, projetil3]} ~=? atualizaDuracaoProjeteisInimigos inimigo1
    ] 

--atualizaTorres
teste17 :: Test
teste17 =
  TestLabel "Testes para a função atualizaTorres" $ 
   test
    [
      "Recebe um jogo no estado inicial (sem torres)" ~: jogoInicial ~=? atualizaTorres jogoInicial,
      "Recebe um jogo com torres" ~: jogoInicial {torresJogo = [torreA3, torreB3]} ~=? atualizaTorres jogoInicial {torresJogo = [torreA3, torreB3]},
      "Recebe um jogo com torres, e com inimigos" ~: jogoInicial {torresJogo = [torreA3 {tempoTorre = 2}, torreB3 {tempoTorre = 2}], inimigosJogo = [inimigoF3 {vidaInimigo = -2, 
                                                                                                                                                                projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}]},
                                                                                                                                                     inimigoE3 {vidaInimigo = 0, 
                                                                                                                                                                projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.0}]},
                                                                                                                                                     inimigoA3 {vidaInimigo = 2, 
                                                                                                                                                                projeteisInimigo = [Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}]}]}
                                                  ~=? atualizaTorres jogoInicial {torresJogo = [torreA3, torreB3], inimigosJogo = [inimigoE3, inimigoA3, inimigoF3]}
    ]

--atualizaInimigosEBase
teste18 :: Test 
teste18 = 
  TestLabel "Testes para a função atualizaInimigosEBase" $
   test 
    [
      "Recebe um jogo no estado inicial" ~: jogoInicial {acGeraCaminhos = 1} ~=? atualizaInimigosEBase 1 jogoInicial, 
      "Recebe um jogo com inimigos no mapa, que não morrem, e nem chegam a base" ~: jogoInicial {portaisJogo = [portalB3], 
                                                                                                 baseJogo = baseB, 
                                                                                                 inimigosJogo = [inimigoA3 {posicaoInimigo = (0.1, 0), 
                                                                                                                           direcaoInimigo = Este, 
                                                                                                                           caminhoInimigo = [Este, Norte, Norte, Norte, Norte, Este, Este, Este, Sul, Este, Sul]}, 
                                                                                                                 inimigoB3 {posicaoInimigo = (0.1, 0), 
                                                                                                                            direcaoInimigo = Este, 
                                                                                                                            caminhoInimigo = [Este, Norte, Norte, Norte, Norte, Este, Este, Este, Este, Sul, Sul], acDirecao = (0,0)}], acGeraCaminhos = 1} 
                                                                                 ~=? atualizaInimigosEBase 0.1 jogoInicial {portaisJogo = [portalB3], 
                                                                                                                          inimigosJogo = [inimigoA3, inimigoB3 {posicaoInimigo = (0,0), acDirecao = (0,0)}],
                                                                                                                          baseJogo = baseB}, 
      "Recebe um jogo, e o inimigo atinge a base" ~: jogoInicial {portaisJogo = [portalB3], 
                                                                  baseJogo = baseB {vidaBase = 45}, 
                                                                  inimigosJogo = [inimigoA3 {posicaoInimigo = (0.1, 0), 
                                                                                             direcaoInimigo = Este, 
                                                                                             caminhoInimigo = [Este, Norte, Norte, Norte, Norte, Este, Este, Este, Sul, Este, Sul]} 
                                                                                 ], acGeraCaminhos = 1}
                                                  ~=? atualizaInimigosEBase 0.1 jogoInicial {portaisJogo = [portalB3], 
                                                                                             inimigosJogo = [inimigoA3, 
                                                                                                             inimigoB3 {posicaoInimigo = (5, 2), caminhoInimigo = []}],
                                                                                             baseJogo = baseB},
      "Recebe um jogo, e o inimigo morre" ~: jogoInicial {portaisJogo = [portalB3], 
                                                          torresJogo = [torreB3 {tempoTorre = 0}],
                                                          baseJogo = baseB {creditosBase = 15}, 
                                                          inimigosJogo = [], acGeraCaminhos = 1}
                                          ~=? atualizaInimigosEBase 0.1 jogoInicial {portaisJogo = [portalB3], 
                                                                                     torresJogo = [torreB3 {tempoTorre = 0}],
                                                                                     inimigosJogo = [inimigoA3 {posicaoInimigo = (1,3), vidaInimigo = 0}],
                                                                                     baseJogo = baseB}
    ]

--atualizaJogo
teste19 :: Test 
teste19 = 
  TestLabel "Testes para a função atualizaJogo" $
   test 
    [
      "Recebe um jogo no estado inicial" ~: jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {inimigosOnda = [], cicloOnda = 3.0, tempoOnda = 3.0, entradaOnda = 0.0}, ondaB3]}],
                                                         inimigosJogo = [inimigoA3 {posicaoInimigo = (1, 0), caminhoInimigo = []}], 
                                                         baseJogo = baseB, acGeraCaminhos = 1}
                                        ~=? atualizaJogo 0.1 jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {entradaOnda = 0, inimigosOnda = [inimigoA3 {posicaoInimigo = (1,0)}]}, ondaB3]}],
                                                                          inimigosJogo = [], 
                                                                          baseJogo = baseB}, 
     "Recebe o jogo acima 1" ~: jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {inimigosOnda = [], cicloOnda = 3.0, tempoOnda = 2.0, entradaOnda = 0.0}, ondaB3]}],
                                           inimigosJogo = [inimigoA3 {posicaoInimigo = (1, 0.2), caminhoInimigo = [Norte, Norte, Norte, Este, Este, Este, Sul, Este, Sul], iteracoesDesdeInicioAnimacaoInimigo = 2, acDirecao = (1,0)}], 
                                           baseJogo = baseB, acGeraCaminhos = 1}
                          ~=? atualizaJogo 0.2 jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {inimigosOnda = [], cicloOnda = 3.0, tempoOnda = 3.0, entradaOnda = 0.0}, ondaB3]}],
                                                            inimigosJogo = [inimigoA3 {posicaoInimigo = (1, 0), caminhoInimigo = []}], 
                                                            baseJogo = baseB},
      "Recebe o jogo acima 2" ~: jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {inimigosOnda = [], cicloOnda = 3.0, tempoOnda = 1.0, entradaOnda = 0.0}, ondaB3]}],
                                           inimigosJogo = [inimigoA3 {posicaoInimigo = (1.0,2.2), caminhoInimigo = [Norte, Norte, Este, Este, Este, Sul, Este, Sul], iteracoesDesdeInicioAnimacaoInimigo = 3, acDirecao = (1,2)}], 
                                           baseJogo = baseB, acGeraCaminhos = 1}
                            ~=? atualizaJogo 0.2 jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {inimigosOnda = [], cicloOnda = 3.0, tempoOnda = 2.0, entradaOnda = 0.0}, ondaB3]}],
                                           inimigosJogo = [inimigoA3 {posicaoInimigo = (1, 2), caminhoInimigo = [Norte, Norte, Norte, Este, Este, Este, Sul, Este, Sul], iteracoesDesdeInicioAnimacaoInimigo = 2, acDirecao = (1,0)}], 
                                           baseJogo = baseB},
      "Recebe o jogo acima 3" ~: jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {inimigosOnda = [], cicloOnda = 3.0, tempoOnda = 0.0, entradaOnda = 0.0}, ondaB3]}],
                                           inimigosJogo = [inimigoA3 {posicaoInimigo = (1.0,4.2), caminhoInimigo = [Norte, Este, Este, Este, Sul, Este, Sul], iteracoesDesdeInicioAnimacaoInimigo =4, acDirecao = (1,4)}], 
                                           baseJogo = baseB, acGeraCaminhos = 1}
                            ~=? atualizaJogo 0.2 jogoInicial {portaisJogo = [portalA3 {ondasPortal = [ondaA3 {inimigosOnda = [], cicloOnda = 3.0, tempoOnda = 1.0, entradaOnda = 0.0}, ondaB3]}],
                                           inimigosJogo = [inimigoA3 {posicaoInimigo = (1, 4), caminhoInimigo = [Norte, Norte, Este, Este, Este, Sul, Este, Sul], iteracoesDesdeInicioAnimacaoInimigo = 3, acDirecao = (1,2)}], 
                                           baseJogo = baseB},
      "Teste com um inimigo com vida nula" ~: (baseJogo jogoJ) {creditosBase = 650} ~=? baseJogo (atualizaJogo 1 jogoJ),
      "Teste com um inimigo com vida negativa" ~: (baseJogo jogoH) {creditosBase = 650} ~=? baseJogo (atualizaJogo 1 jogoH),
      "Teste com mais de um inimigo" ~: (baseJogo jogoI) {creditosBase = 800} ~=? baseJogo (atualizaJogo 1 jogoI)
    ]                       

--atualizaAnimaçãoTorres
teste20 :: Test 
teste20 = 
  TestLabel "Testes para a função atualizaAnimaçãoTorres" $
   test 
    [
      "Torre sem animação com inimigo no alcance e tempo 0" ~: jogoInicial {inimigosJogo = [inimigoA3 {posicaoInimigo = (5,1)}], torresJogo = [torreA3 {iteracoesDesdeInicioAnimacao = 2}]} ~=? atualizaAnimacaoTorres jogoInicial {inimigosJogo = [inimigoA3 {posicaoInimigo = (5,1)}], torresJogo = [torreA3]},
      "Torre com animação a meio" ~: jogoInicial {torresJogo = [torreB3 {iteracoesDesdeInicioAnimacao = 3}]} ~=? atualizaAnimacaoTorres jogoInicial {torresJogo = [torreB3]},
      "Torre com animação no final" ~: jogoInicial {torresJogo = [torreC3 {iteracoesDesdeInicioAnimacao = 1}]} ~=? atualizaAnimacaoTorres jogoInicial {torresJogo = [torreC3]},
      "Torre sem animação e sem inimigo no alcance" ~: jogoInicial {inimigosJogo = [inimigoA3 {posicaoInimigo = (15,15)}], torresJogo = [torreA3]} ~=? atualizaAnimacaoTorres jogoInicial {inimigosJogo = [inimigoA3 {posicaoInimigo = (15,15)}], torresJogo = [torreA3]}
    ]

--atualizaAnimaçãoInimigos
teste21 :: Test 
teste21 = 
  TestLabel "Testes para a função atualizaAnimaçãoInimigos" $
   test 
    [
      "Inimigo com velocidade nula" ~: jogoInicial {inimigosJogo = [inimigoA3 {iteracoesDesdeInicioAnimacaoInimigo = 0, velocidadeInimigo = 0}]} ~=? atualizaAnimacaoInimigos jogoInicial {inimigosJogo = [inimigoA3 {velocidadeInimigo = 0}]},
      "Inimigo com animação a meio" ~: jogoInicial {inimigosJogo = [inimigoA3 {iteracoesDesdeInicioAnimacaoInimigo = 2}]} ~=? atualizaAnimacaoInimigos jogoInicial {inimigosJogo = [inimigoA3]},
      "Inimigo com animação no final" ~: jogoInicial {inimigosJogo = [inimigoB3 {iteracoesDesdeInicioAnimacaoInimigo = 1}]} ~=? atualizaAnimacaoInimigos jogoInicial {inimigosJogo = [inimigoB3]}
    ]

--a função geraCaminhos apenas corre quando o mapa é válido, ou seja, não há necessidade de um caso de teste para um caso em que não exista um caminho
teste22 :: Test
teste22 = 
  TestLabel "Testes para a função geraCaminhos" $
   test 
    [
      "teste 1" ~: [Inimigo {tipoInimigo = Mulher, 
                                      projeteisInimigo = [], 
                                      vidaInimigo = 0, 
                                      butimInimigo = 150,  
                                      ataqueInimigo = 20, 
                                      velocidadeInimigo = 1,
                                      caminhoInimigo = [Norte,Este,Este,Norte,Norte,Norte,Norte,Norte,Oeste,Norte,Norte,Norte,Norte,Este,Este,Este,Este,Este,Este,Este,Sul,Sul,Este,Este],
                                      iteracoesDesdeInicioAnimacaoInimigo = 1,
                                      posicaoInimigo = (5,1),
                                      acDirecao = (5,1),
                                      direcaoInimigo = Norte
                                      }] ~=? geraCaminhos (inimigosJogo jogoJ) mapaJ baseJ 0,
      "teste 2" ~: [Inimigo {tipoInimigo = Mulher, 
                                      projeteisInimigo = [], 
                                      vidaInimigo = 0, 
                                      butimInimigo = 150,  
                                      ataqueInimigo = 20, 
                                      velocidadeInimigo = 1,
                                      caminhoInimigo = [Norte,Norte,Oeste,Oeste,Oeste,Oeste,Sul,Sul,Sul,Sul,Oeste],
                                      iteracoesDesdeInicioAnimacaoInimigo = 1,
                                      posicaoInimigo = (5,2),
                                      acDirecao = (5,2),
                                      direcaoInimigo = Norte
                                      }] ~=? geraCaminhos [Inimigo {tipoInimigo = Mulher, 
                                                                    projeteisInimigo = [], 
                                                                    vidaInimigo = 0, 
                                                                    butimInimigo = 150,  
                                                                    ataqueInimigo = 20, 
                                                                    velocidadeInimigo = 1,
                                                                    caminhoInimigo = [],
                                                                    iteracoesDesdeInicioAnimacaoInimigo = 1,
                                                                    posicaoInimigo = (5,2),
                                                                    acDirecao = (5,2)
                                                                    }] mapaA baseJ {posicaoBase = (0,0)} 0
    ]

teste23 :: Test
teste23 = 
  TestLabel "Testes para a função moveInimigo" $
   test 
    [
      "inimigo não tem caminho" ~: inimigoA3 ~=? moveInimigo inimigoA3,
      "inimigo tem apenas mais uma direção no caminho e está pronto a trocar de direção" ~: inimigoA3 {caminhoInimigo = [Sul], posicaoInimigo = (0,1), acDirecao = (0,1), direcaoInimigo = Sul} ~=? moveInimigo inimigoA3 {caminhoInimigo = [Sul], posicaoInimigo = (0,1), acDirecao = (0,0)},
      "inimigo tem apenas mais uma direção no caminho e não está pronto a trocar de direção" ~: inimigoA3 {caminhoInimigo = [Sul], posicaoInimigo = (0,0.5), acDirecao = (0,0)} ~=? moveInimigo inimigoA3 {caminhoInimigo = [Sul], posicaoInimigo = (0,0.5), acDirecao = (0,0)},
      "inimigo tem mais de uma direção no caminho e está pronto a trocar de direção" ~: inimigoA3 {caminhoInimigo = [Oeste, Este], posicaoInimigo = (0,1), acDirecao = (0,1), direcaoInimigo = Oeste} ~=? moveInimigo inimigoA3 {caminhoInimigo = [Sul, Oeste, Este], posicaoInimigo = (0,1), acDirecao = (0,0), direcaoInimigo = Sul},
      "inimigo tem mais de uma direção no caminho e não está pronto a trocar de direção" ~: inimigoA3 {caminhoInimigo = [Sul, Oeste, Este], posicaoInimigo = (0,0.5), acDirecao = (0,0)} ~=? moveInimigo inimigoA3 {caminhoInimigo = [Sul, Oeste, Este], posicaoInimigo = (0,0.5), acDirecao = (0,0)}
    ]



jogoJ :: Jogo
jogoJ = Jogo {baseJogo = baseJ,
              torresJogo = [],
              portaisJogo = [portal1_J],
              mapaJogo = mapaJ,
              inimigosJogo = [Inimigo {tipoInimigo = Mulher, 
                                                                  projeteisInimigo = [], 
                                                                  vidaInimigo = 0, 
                                                                  butimInimigo = 150,  
                                                                  ataqueInimigo = 20, 
                                                                  velocidadeInimigo = 1,
                                                                  caminhoInimigo = [],
                                                                  iteracoesDesdeInicioAnimacaoInimigo = 1,
                                                                  posicaoInimigo = (5,1),
                                                                  acDirecao = (5,1){- ,
                                                                  direcaoInimigo = Sul -}
                                                                  }],
              lojaJogo = lojaJ
            }

jogoH :: Jogo
jogoH = Jogo {baseJogo = baseJ,
              torresJogo = [],
              portaisJogo = [portal1_J],
              mapaJogo = mapaJ,
              inimigosJogo = [Inimigo {tipoInimigo = Mulher, 
                                                                  projeteisInimigo = [], 
                                                                  vidaInimigo = -10, 
                                                                  butimInimigo = 150,  
                                                                  ataqueInimigo = 20, 
                                                                  velocidadeInimigo = 1,
                                                                  caminhoInimigo = [],
                                                                  iteracoesDesdeInicioAnimacaoInimigo = 1,
                                                                  posicaoInimigo = (5,1),
                                                                  acDirecao = (5,1){- ,
                                                                  direcaoInimigo = Sul -}
                                                                  }],
              lojaJogo = lojaJ
            }

jogoI :: Jogo
jogoI = Jogo {baseJogo = baseJ,
              torresJogo = [],
              portaisJogo = [portal1_J],
              mapaJogo = mapaJ,
              inimigosJogo = [Inimigo {tipoInimigo = Mulher, 
                                        projeteisInimigo = [], 
                                        vidaInimigo = 0, 
                                        butimInimigo = 150,  
                                        ataqueInimigo = 20, 
                                        velocidadeInimigo = 1,
                                        caminhoInimigo = [],
                                        iteracoesDesdeInicioAnimacaoInimigo = 1,
                                        posicaoInimigo = (5,1),
                                        acDirecao = (5,1){- ,
                                        direcaoInimigo = Sul -}
                                        },
                              Inimigo {tipoInimigo = Mulher, 
                                        projeteisInimigo = [], 
                                        vidaInimigo = -10, 
                                        butimInimigo = 150,  
                                        ataqueInimigo = 20, 
                                        velocidadeInimigo = 1,
                                        caminhoInimigo = [],
                                        iteracoesDesdeInicioAnimacaoInimigo = 1,
                                        posicaoInimigo = (5,1),
                                        acDirecao = (5,1){- ,
                                        direcaoInimigo = Sul -}
                                        },
                              Inimigo {tipoInimigo = Mulher, 
                                        projeteisInimigo = [], 
                                        vidaInimigo = 10, 
                                        butimInimigo = 150,  
                                        ataqueInimigo = 20, 
                                        velocidadeInimigo = 1,
                                        caminhoInimigo = [],
                                        iteracoesDesdeInicioAnimacaoInimigo = 1,
                                        posicaoInimigo = (5,1),
                                        acDirecao = (5,1){- ,
                                        direcaoInimigo = Sul -}
                                        }],
              lojaJogo = lojaJ
            }

lojaJ :: Loja
lojaJ = [ {- (100, Torre{projetilTorre = Projetil {tipoProjetil = Gelo}}),
         (150, Torre{projetilTorre = Projetil {tipoProjetil = Resina}}),
         (200, Torre{projetilTorre = Projetil {tipoProjetil = Fogo}}) -}
        ]

baseJ :: Base
baseJ = baseTds {posicaoBase = (15,9)}

portal1_J :: Portal
portal1_J = Portal {posicaoPortal = (0,9),
                    ondasPortal = [Onda {inimigosOnda = [],
                                        cicloOnda = 5*60,
                                        tempoOnda = 10*60,
                                        entradaOnda = 0}]
                    }

mapaJ :: Mapa 
mapaJ = 
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

mapaA :: Mapa
mapaA =
 [ [t,t,r,a,a,a,r,r,r,r,r,r,r,r,r,r],
   [r,t,r,a,r,r,r,r,r,r,r,r,r,r,r,r],
   [r,t,r,a,r,t,r,r,r,r,r,r,r,r,r,r],
   [r,t,r,a,t,t,r,r,r,r,r,r,r,r,r,r],
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
baseB :: Base 
baseB = Base 
  { 
    vidaBase = 50.0,
    posicaoBase = (5, 2),
    creditosBase = 10
  }

jogoInicial :: Jogo 
jogoInicial = Jogo 
 {
  baseJogo = baseA ,
  portaisJogo = [portalA3],
  torresJogo = [],
  mapaJogo = mapaA, 
  inimigosJogo = [], 
  lojaJogo = lojaA,
  acGeraCaminhos = 0
 }
lojaA :: Loja
lojaA = [(50, torreA3)]

-- Torre com gelo e sem animação
torreA3 :: Torre
torreA3 = Torre 
 {
    posicaoTorre = (5,1),
    danoTorre = 6,
    alcanceTorre = 3,
    rajadaTorre = 3,
    cicloTorre = 2,
    tempoTorre = 0,
    projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1},
    iteracoesDesdeInicioAnimacao = 1
 }

-- Torre com resina e a meio da animação
torreB3 :: Torre 
torreB3 = Torre 
 {
    posicaoTorre = (2,3),
    danoTorre = 4,
    alcanceTorre = 5,
    rajadaTorre = 3,
    cicloTorre = 2,
    tempoTorre = 0,
    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
    iteracoesDesdeInicioAnimacao = 2
 }

--torre no final da animação
torreC3 :: Torre 
torreC3 = Torre 
 {
    posicaoTorre = (2,3),
    danoTorre = 4,
    alcanceTorre = 5,
    rajadaTorre = 3,
    cicloTorre = 2,
    tempoTorre = 0,
    projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
    iteracoesDesdeInicioAnimacao = 29
 }


portalA3 :: Portal 
portalA3 = Portal 
 {
    posicaoPortal = (1, 0),
    ondasPortal = [ondaA3, ondaB3] 
 }

portalB3 :: Portal 
portalB3 = Portal 
 {
    posicaoPortal = (0, 0),
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
  posicaoInimigo = (0,0),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  velocidadeInimigo = 1.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (0,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1  
 }

inimigoB3 :: Inimigo 
inimigoB3 = Inimigo 
 {
  posicaoInimigo = (1, 0),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 1.0,
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (1,0),
  iteracoesDesdeInicioAnimacaoInimigo = 32  
 }

inimigoC3 :: Inimigo 
inimigoC3 = Inimigo 
 {
  posicaoInimigo = (0, 0),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = [],
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (0,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1  
 }

inimigoD3 :: Inimigo 
inimigoD3 = Inimigo 
 {
  posicaoInimigo = (1, 0),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (1,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1  
 }

inimigoE3 :: Inimigo 
inimigoE3 = Inimigo 
 {
  posicaoInimigo = (5, 2),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (5,2),
  iteracoesDesdeInicioAnimacaoInimigo = 1 
 }

inimigoF3 :: Inimigo 
inimigoF3 = Inimigo 
 {
  posicaoInimigo = (2, 4),
  direcaoInimigo = Norte,
  vidaInimigo = 2.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (2,4),
  iteracoesDesdeInicioAnimacaoInimigo = 1 
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
  posicaoInimigo = (5,4),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 0.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [projetil1, projetil2, projetil3],
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (5,4),
  iteracoesDesdeInicioAnimacaoInimigo = 1  
 }

--inimigo com velocidade não nula, não afetado por nenhum projetil e com posição diferente da base1
inimigo2 :: Inimigo 
inimigo2 = Inimigo 
 {
  posicaoInimigo = (0,0),
  direcaoInimigo = Este,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [], 
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (0,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1  
 }

--inimigo com vida nula, butim 5 e com posição igual à base1
inimigo3 :: Inimigo 
inimigo3 = Inimigo 
 {
  posicaoInimigo = (5,4),
  direcaoInimigo = Sul,
  vidaInimigo = 0.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [],
  tipoInimigo = Mulher,
  caminhoInimigo = [],
  acDirecao = (5,4),
  iteracoesDesdeInicioAnimacaoInimigo = 1   
 }

--inimigo com vida nula, butim 10 e com posição diferente da base1
inimigo4 :: Inimigo 
inimigo4 = Inimigo 
 {
  posicaoInimigo = (0,0),
  direcaoInimigo = Norte,
  vidaInimigo = 0.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 10, 
  projeteisInimigo = [], 
  tipoInimigo = Mulher , 
  caminhoInimigo = [],
  acDirecao = (0,0),
  iteracoesDesdeInicioAnimacaoInimigo = 1  
 }

baseA :: Base 
baseA = Base 
  { 
    vidaBase = 50.0,
    posicaoBase = (5, 4),
    creditosBase = 10
  }