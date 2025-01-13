module FuncoesAuxSpec (testesFuncoesAux) where

import FuncoesAux
import LI12425
import Test.HUnit

testesFuncoesAux:: Test
testesFuncoesAux =
  TestLabel "Testes Funções Auxiliares" $
    test
      [teste1, teste2, teste3, teste4, teste5] 

teste1 :: Test 
teste1 = 
  TestLabel "Testes para a função geraOndasPortal" $
   test 
    [
      "apenas um inimigo 1 e dois inimigos 2" ~: [ondaGOP {inimigosOnda = [inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                                           inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                                           inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)}], tempoOnda = 0},
                                                  ondaGOP {inimigosOnda = [inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                                           inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                                           inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)}]}] ~=? escolheDirecao (geraOndasPortal 2 1 2 (0,9)),
      "apenas inimigos 1" ~: [ondaGOP {inimigosOnda = [inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                       inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                       inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)}], tempoOnda = 0},
                              ondaGOP {inimigosOnda = [inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                       inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                       inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)}]}] ~=? escolheDirecao (geraOndasPortal 2 3 0 (0,9)),
      "sem inimigos" ~: [ondaGOP {inimigosOnda = [], tempoOnda = 0},
                        ondaGOP {inimigosOnda = []}] ~=? escolheDirecao (geraOndasPortal 2 0 0 (0,9)),
      "inimigos 1 e inimigos 2" ~: [ondaGOP {inimigosOnda = [inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)}], tempoOnda = 0},
                                    ondaGOP {inimigosOnda = [inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo2GOP {posicaoInimigo = (0,9), acDirecao = (0,9)},
                                                             inimigo1GOP {posicaoInimigo = (0,9), acDirecao = (0,9)}]}] ~=? escolheDirecao (geraOndasPortal 2 3 2 (0,9))
    ]

teste2 :: Test 
teste2 = 
  TestLabel "Testes para a função atualizaMapa" $
   test 
    [
        "Atualiza o mapa com relva" ~: [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] 
                                    ~=? atualizaMapa ((5,5), Relva) [((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)],
        "Atualiza mapa com Agua" ~: [((7,7), Agua), ((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] 
                                ~=? atualizaMapa ((7,7), Agua) [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)],
        "Atualiza mapa com Terra" ~: [((3,8), Terra),((7,7), Agua), ((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)]
                               ~=? atualizaMapa ((3,8), Terra) [((3,8), Terra), ((7,7), Agua), ((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)],
        "Atualiza com posição já existente" ~: [((0,0), Relva), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] 
                                             ~=? atualizaMapa ((0,0), Relva) [((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)],
        "A lista esta vazia" ~: [((5,5), Terra)] ~=? atualizaMapa ((5,5), Terra) []                        

    ]

teste3 :: Test 
teste3 = 
  TestLabel "Testes para a função atualizaMapa" $
   test 
    [
        "Transforma em Mapa" ~: [[Terra, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva], 
                                [Relva, Agua, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Terra, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva],
                                [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva]]
                              ~=? transformaMapa [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)]
                   

    ]

teste4 :: Test 
teste4 = 
  TestLabel "Testes para a função adicionarPortais" $
   test 
    [
        "Adiciona o portal" ~: [Portal {posicaoPortal = (0,0), ondasPortal = []}]
                              ~=? adicionarPortais Portal {posicaoPortal = (0,0), ondasPortal = []} [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] [],
        "Não adiciona Portal, posicao relva"~: []
                              ~=? adicionarPortais Portal {posicaoPortal = (5,5), ondasPortal = []} [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] [],
        "Não adiciona Portal, posicao agua"~: []
                              ~=? adicionarPortais Portal {posicaoPortal = (1,1), ondasPortal = []} [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] [],                      
        "Não adiciona Portal, não há terreno na posição"~: []
                              ~=? adicionarPortais Portal {posicaoPortal = (1,0), ondasPortal = []} [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] [],
        "Adiciona Portal" ~:  [Portal {posicaoPortal = (0,0), ondasPortal = []}, Portal {posicaoPortal = (8,8), ondasPortal = []}] 
                         ~=? adicionarPortais Portal {posicaoPortal = (0,0), ondasPortal = []} [((5,5), Relva), ((0,0), Terra), ((1,1), Agua), ((2,2), Relva), ((8,8),Terra)] [ Portal {posicaoPortal = (8,8), ondasPortal = []}]
     ]

teste5 :: Test 
teste5 = 
  TestLabel "Testes para a função deletePortais" $
   test 
    [
        "Deleta o portal" ~: [Portal {posicaoPortal = (0,0), ondasPortal = []}, Portal {posicaoPortal = (8,8), ondasPortal = []}]
                              ~=? deletePortal [Portal {posicaoPortal = (0,0), ondasPortal = []}, Portal {posicaoPortal = (8,8), ondasPortal = []}, Portal {posicaoPortal = (7,6), ondasPortal = []}] (7,6),
        "Não deleta o portal" ~:  [Portal {posicaoPortal = (0,0), ondasPortal = []}, Portal {posicaoPortal = (8,8), ondasPortal = []}, Portal {posicaoPortal = (7,6), ondasPortal = []}]
                             ~=? deletePortal [Portal {posicaoPortal = (0,0), ondasPortal = []}, Portal {posicaoPortal = (8,8), ondasPortal = []}, Portal {posicaoPortal = (7,6), ondasPortal = []}] (5,6)
    ]
       

-- Esta função serve para conseguir fazer teste da geraOndasPortal, dado que a geraOndasPortal não atribui uma direção aos inimigos, e, embora isso não seja um problema para o jogo, dado que a geraCaminhos faz isso, para correr os testes é necessário que os inimigos tenham direções

escolheDirecao :: [Onda] -> [Onda]
escolheDirecao [] = []
escolheDirecao (o:os) =
  let is = inimigosOnda o
  in o {inimigosOnda = map (\i -> i {direcaoInimigo = Norte}) is} : escolheDirecao os

ondaGOP :: Onda
ondaGOP = Onda {cicloOnda = 5*60,
                tempoOnda = 10*60,
                entradaOnda = 0
                }

inimigo1GOP :: Inimigo
inimigo1GOP = Inimigo {tipoInimigo = Homem, 
                        projeteisInimigo = [], 
                        vidaInimigo = 150, 
                        butimInimigo = 50, 
                        ataqueInimigo = 40, 
                        velocidadeInimigo = 0.5, 
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1,
                        direcaoInimigo = Norte}

inimigo2GOP :: Inimigo
inimigo2GOP = Inimigo {tipoInimigo = Mulher, 
                        projeteisInimigo = [], 
                        vidaInimigo = 100, 
                        butimInimigo = 45,  
                        ataqueInimigo = 20, 
                        velocidadeInimigo = 1,
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1,
                        direcaoInimigo = Norte}
