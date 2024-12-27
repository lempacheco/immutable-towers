module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8, teste9, teste10]

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

-- inimigosSobreviventesAlcance

teste4 :: Test
teste4 =
  TestLabel "Testes para a função inimigosSobreviventesAlcance" $
    test
      [ "Recebe uma lista vazia de inimigos" ~: [] ~=? inimigosSobreviventesAlcance torreA3 [],
        "Um inimigo morre" ~: [inimigoA3, 
                               inimigoA3 {posicaoInimigo = (5.5,3.5), 
                                          vidaInimigo = 2.0, 
                                          projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}] ~=? inimigosSobreviventesAlcance torreA3 [inimigoA3, inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 6.0}, 
                                                                                                                                                                      inimigoA3{posicaoInimigo = (5.5,3.5), vidaInimigo = 10.0}], 
        "Tem inimigos sobreviventes" ~: [inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}, 
                                         inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 10.0, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}]}] 
                                     ~=? inimigosSobreviventesAlcance torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 16.0}, 
                                                                        inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 8}, 
                                                                        inimigoA3], 
        "Não tem inimigos sobreviventes" ~: [] ~=? inimigosSobreviventesAlcance torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 4.0}, 
                                                                                         inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0}], 
        "Só tem inimigos sobreviventes fora do alcance da torre" ~: [inimigoA3, 
                                                                     inimigoA3 {posicaoInimigo = (1.5,0.5)}, 
                                                                     inimigoA3 {posicaoInimigo = (1.5, 1.5)}] ~=? inimigosSobreviventesAlcance torreA3 [inimigoA3 {posicaoInimigo = (5.5,3.5), vidaInimigo = 4.0}, 
                                                                                                                                                        inimigoA3 {posicaoInimigo = (5.5,2.5), vidaInimigo = 2.0}, 
                                                                                                                                                        inimigoA3, 
                                                                                                                                                        inimigoA3 {posicaoInimigo = (1.5,0.5)}, 
                                                                                                                                                        inimigoA3 {posicaoInimigo = (1.5, 1.5)}]
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
                                                                torreA3 {tempoTorre = 2.0}) 
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
        inimigo1 {vidaInimigo = 5.0} ~=? atualizaInimigoFogo inimigo1
      ]

teste10 :: Test
teste10 = 
  TestLabel "Teste para a função inimigosSemVida" $
    test
      [
        "Teste só com inimigos com vida" ~: (portal1, base1 {creditosBase = 25}) ~=? inimigosSemVida portal1 base1
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
       a = Aguas

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

portalA3 :: Portal 
portalA3 = Portal 
 {
    posicaoPortal = (1.5, 0.5),
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

inimigoB3 :: Inimigo 
inimigoB3 = Inimigo 
 {
  posicaoInimigo = (1.5, 0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 6.0,
  ataqueInimigo = 5.0, 
  velocidadeInimigo = 0.0,
  butimInimigo = 5, 
  projeteisInimigo = []
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
    duracaoProjetil = Finita 2.0
  }

--projetil de tipo Resina
projetil3 :: Projetil 
projetil3 = Projetil 
 {tipoProjetil = Resina, 
  duracaoProjetil = Infinita
 }

--inimigo com velocidade nula
inimigo1 :: Inimigo 
inimigo1 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 0.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [] 
 }

--inimigo com velocidade não nula
inimigo2 :: Inimigo 
inimigo2 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 10.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [] 
 }

--inimigo com vida nula e butim 5
inimigo3 :: Inimigo 
inimigo3 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 0.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 5, 
  projeteisInimigo = [] 
 }

--inimigo com vida nula e butim 10
inimigo4 :: Inimigo 
inimigo4 = Inimigo 
 {
  posicaoInimigo = (0.5,0.5),
  direcaoInimigo = Norte,
  vidaInimigo = 0.0,
  velocidadeInimigo = 10.0,
  ataqueInimigo = 5.0, 
  butimInimigo = 10, 
  projeteisInimigo = [] 
 }

--portal apenas com inimigos com vida
portal1 :: Portal 
portal1 = Portal 
  { 
    posicaoPortal = (0.5, 0.5),
    ondasPortal = [onda1] 
  }

onda1 :: Onda
onda1 = Onda
  {
    inimigosOnda = [inimigo1, inimigo2],
    cicloOnda = 10,
    tempoOnda = 5,
    entradaOnda = 9
  }

--portal apenas com inimigos sem vida
portal2 :: Portal 
portal2 = Portal 
  { 
    posicaoPortal = (0.5, 0.5),
    ondasPortal = [onda2] 
  }

onda2 :: Onda
onda2 = Onda
  {
    inimigosOnda = [inimigo3, inimigo4],
    cicloOnda = 10,
    tempoOnda = 5,
    entradaOnda = 9
  }

--portal com inimigos com e sem vida
portal3 :: Portal 
portal3 = Portal 
  { 
    posicaoPortal = (0.5, 0.5),
    ondasPortal = [onda1, onda2] 
  }

base1 :: Base 
base1 = Base 
  { 
    vidaBase = 5,
    posicaoBase = (5.5, 4.5),
    creditosBase = 10
  }
