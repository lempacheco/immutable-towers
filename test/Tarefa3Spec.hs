module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ teste1, teste2, teste3, teste4, teste5 ]

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
        "Não tem inimigos no alcance" ~: [] ~=? inimigosOrdenados torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1.5,1.5)}], 
        "Tem inimigos no alcance" ~: [inimigoA3 {posicaoInimigo = (5.5,2.5)}, inimigoA3 {posicaoInimigo = (5.5,3.5)} ] ~=? inimigosOrdenados torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5)}, 
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

-- inimigosSobreviventes

teste4 :: Test
teste4 =
  TestLabel "Testes para a função inimigosSobreviventes" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? inimigosSobreviventes torreA3 [],
        "Não tem inimigos sobreviventes" ~: [] ~=? inimigosSobreviventes torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 6.0}], 
        "Tem inimigos sobreviventes" ~: [inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                         inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 10.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}] 
                                     ~=? inimigosSobreviventes torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 16.0}, 
                                                                        inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 8}, 
                                                                        inimigoA3]
      ]

-- dispaProjeteis 

teste5 :: Test
teste5 =
  TestLabel "Testes para a função disparaProjeteis" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: ([], torreA3) ~=? disparaProjeteis torreA3 [],
        "Não tem inimigos no alcance" ~: ([], torreA3) ~=? disparaProjeteis torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (1.5,1.5)}], 
        "A torre não está pronta para disparar" ~: ([inimigoA3 {posicaoInimigo = (5.5,3.5)}, 
                                                     inimigoA3 {posicaoInimigo = (5.5,2.5)},
                                                     inimigoA3 {posicaoInimigo = (5.5,4.5)}, 
                                                     inimigoA3 {posicaoInimigo = (4.5,4.5)}], torreA3 {tempoTorre = 1}) ~=? disparaProjeteis torreA3 {tempoTorre = 2} [inimigoA3 {posicaoInimigo = (5.5,3.5)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (5.5,2.5)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (5.5,4.5)}, 
                                                                                                                                                                       inimigoA3 {posicaoInimigo = (4.5,4.5)}], 
        "A torre está pronta para disparar" ~: ([inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                                 inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 3.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]},  
                                                 inimigoA3 {posicaoInimigo = (4.5,3.5), vidaInimigo = 4.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                                 inimigoA3 {posicaoInimigo = (5.5,4.5), vidaInimigo = 8.0}],
                                                 torreA3 {tempoTorre = 2.0}) 
                                            ~=? disparaProjeteis torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 9.0}, 
                                                                          inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (5.5,4.5), vidaInimigo = 8.0}, 
                                                                          inimigoA3 {posicaoInimigo = (4.5,3.5), vidaInimigo = 10.0}, inimigoA3]
      ]



{-
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

-}

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

inimigoA3 :: Inimigo 
inimigoA3 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [] 
 }
