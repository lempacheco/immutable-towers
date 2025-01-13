module ElementosDoJogo where

import LI12425
import FuncoesAux

-- loja do jogo. 
loja :: Loja
loja = [ (100, Torre{projetilTorre = Projetil {tipoProjetil = Gelo}}),
         (150, Torre{projetilTorre = Projetil {tipoProjetil = Resina}}),
         (200, Torre{projetilTorre = Projetil {tipoProjetil = Fogo}})
        ]

-- Nível 1

jogo1 :: Jogo
jogo1 = Jogo {baseJogo = base1,
              torresJogo = [],
              portaisJogo = [portal1_1, portal2_1],
              mapaJogo = mapa1,
              inimigosJogo = [],
              lojaJogo = loja,
              acGeraCaminhos = 0
            }

base1 :: Base
base1 = baseTds {posicaoBase = (15,9)}

portal1_1 :: Portal
portal1_1 = Portal {posicaoPortal = (0,9),
                  ondasPortal = geraOndasPortal 1 1 0 (0,9)
                  }

portal2_1 :: Portal
portal2_1 = Portal {posicaoPortal = (5,0), 
                  ondasPortal = geraOndasPortal 1 2 3 (5,0)}

mapa1 :: Mapa 
mapa1 = 
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
    [r,r,r,r,r,r,t,t,t,t,t,t,t,t,r,r],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

-- Nível 2

jogo2 :: Jogo
jogo2 = Jogo {baseJogo = base2,
              torresJogo = [],
              portaisJogo = [portal1_2, portal2_2, portal3_2],
              mapaJogo = mapa2,
              inimigosJogo = [],
              lojaJogo = loja,
              acGeraCaminhos = 0
            } 

mapa2 :: Mapa 
mapa2 = 
  [ [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [t,t,t,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,t,r,r,t,t,t,t,t,t,t,t,t,r,r],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,t,r,r],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,t,r,r],
    [r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,r,r,r,a,a,a,r,r,t],
    [r,r,t,r,r,r,t,r,r,a,a,a,r,t,t,t],
    [r,r,t,r,r,r,t,r,r,a,a,r,r,t,r,r],
    [r,r,t,r,r,r,t,t,t,t,t,t,t,t,r,r],
    [t,t,t,r,r,r,t,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,t,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,t,r,a,a,a,a,r,r,r,r],
    [r,r,r,r,r,r,t,r,a,a,a,a,r,r,r,r]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

base2 :: Base
base2 = baseTds {posicaoBase = (15,7)}

portal1_2 :: Portal
portal1_2 = Portal {posicaoPortal = (0,1),
                  ondasPortal = geraOndasPortal 1 2 4 (0,1)}

portal2_2 :: Portal
portal2_2 = Portal {posicaoPortal = (0,12), 
                  ondasPortal = geraOndasPortal 2 3 3 (0,12)}

portal3_2 :: Portal
portal3_2 = Portal {posicaoPortal = (5,0), 
                  ondasPortal = geraOndasPortal 2 3 2 (5,0)}

-- Nível 3

mapa3 :: Mapa 
mapa3 = 
  [ [a,a,a,r,r,r,t,r,r,t,r,r,r,r,r,r],
    [a,t,t,t,t,r,t,r,r,t,r,t,t,t,t,r],
    [r,t,a,a,t,r,t,r,r,t,r,t,r,r,t,r],
    [r,t,a,a,t,r,t,r,r,t,r,t,r,r,t,r],
    [r,t,t,t,t,t,t,r,r,t,t,t,t,t,t,r],
    [r,r,r,r,t,a,a,a,r,r,r,t,r,r,r,r],
    [r,r,r,r,t,a,a,a,a,r,r,t,r,r,r,r],
    [r,r,r,r,t,r,a,a,a,a,r,t,r,r,r,r],
    [r,r,r,r,t,r,r,a,a,a,a,t,r,r,r,r],
    [r,r,r,r,t,r,r,r,a,a,a,t,r,r,r,r],
    [r,t,t,t,t,t,t,r,r,t,t,t,t,t,t,r],
    [r,t,r,r,t,r,t,r,r,t,r,t,a,a,t,r],
    [r,t,r,r,t,r,t,t,t,t,r,t,a,a,t,r],
    [r,t,t,t,t,r,r,r,t,r,r,t,t,t,t,a],
    [r,r,r,r,r,r,r,r,t,r,r,r,r,r,a,a],
    [r,r,r,r,r,r,r,r,t,r,r,r,r,a,a,a]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

jogo3 :: Jogo 
jogo3 = Jogo {mapaJogo = mapa3, 
              inimigosJogo = [], 
              portaisJogo = [portal2_3, portal1_3], 
              torresJogo = [], 
              baseJogo = base3,
              lojaJogo = loja,
              acGeraCaminhos = 0}

base3 = baseTds {posicaoBase = (8,15)}

portal1_3 :: Portal
portal1_3 = Portal {posicaoPortal = (6,0),
                  ondasPortal = geraOndasPortal 2 5 3 (6,0)}

portal2_3 :: Portal
portal2_3 = Portal {posicaoPortal = (9,0), 
                  ondasPortal = geraOndasPortal 2 3 2 (9,0)}

-- Nivel 4

mapa4 :: Mapa 
mapa4 = 
  [
    [a,t,a,a,a,a,a,a,a,a,a,a,a,a,a,a],
    [a,t,a,a,a,a,a,t,t,t,t,t,t,t,t,a],
    [a,t,a,a,a,a,a,t,a,a,a,a,a,a,t,a],
    [a,t,t,t,t,t,t,t,a,r,r,r,r,a,t,a],
    [a,t,a,a,a,a,a,t,a,r,r,r,r,a,t,a],
    [a,t,a,r,r,r,a,t,a,a,a,a,a,a,t,a],
    [a,t,a,r,r,r,a,t,a,a,t,t,t,t,t,a],
    [a,t,a,a,a,a,a,t,a,a,t,a,a,a,t,a],
    [a,t,t,t,t,t,t,t,t,t,t,a,r,a,t,a],
    [a,a,a,a,t,a,a,a,a,a,t,a,r,a,t,a],
    [a,a,a,a,t,a,r,r,r,a,t,a,a,a,t,a],
    [a,a,a,a,t,a,r,r,r,a,t,t,t,t,t,a],
    [a,a,a,a,t,a,a,a,a,a,t,a,a,a,t,a],
    [a,a,a,a,t,t,t,t,t,t,t,a,a,a,t,a],
    [a,a,a,a,a,a,a,a,a,a,a,a,a,a,t,t],
    [a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

jogo4 :: Jogo 
jogo4 = Jogo {mapaJogo = mapa4, 
              inimigosJogo = [], 
              portaisJogo = [portal1_4], 
              torresJogo = [], 
              baseJogo = base4,
              lojaJogo = loja,
              acGeraCaminhos = 0}

base4 :: Base
base4 = baseTds {posicaoBase = (15,14)}

portal1_4 :: Portal
portal1_4 = Portal {posicaoPortal = (1,0),
                  ondasPortal = geraOndasPortal 6 3 2 (1,0)}

-- Nivel 5

mapa5 :: Mapa 
mapa5 = 
  [
    [t,t,t,a,a,a,a,a,a,a,a,t,a,a,a,a],
    [a,a,t,t,t,t,t,t,a,a,a,t,t,t,t,a],
    [a,a,t,a,a,a,a,t,a,a,a,a,a,a,t,a],
    [a,a,t,a,r,r,a,t,a,r,r,r,r,a,t,a],
    [a,a,t,a,r,r,a,t,a,r,r,r,r,a,t,a],
    [a,a,t,a,a,a,a,t,a,a,a,a,a,a,t,a],
    [a,a,t,t,t,t,t,t,t,t,t,t,t,t,t,a],
    [a,a,a,a,a,a,a,a,a,a,t,a,a,a,t,a],
    [a,a,a,a,a,a,a,a,a,a,t,a,r,a,t,a],
    [a,a,t,t,t,t,t,t,t,t,t,t,t,t,t,a],
    [t,t,t,a,a,a,r,r,r,a,a,a,a,a,t,a],
    [a,a,t,a,a,a,r,r,r,a,a,a,a,a,t,a],
    [a,a,t,a,a,a,a,a,a,a,a,a,a,a,t,a],
    [a,a,t,t,t,t,t,t,a,a,a,a,a,a,t,a],
    [a,a,a,a,a,a,a,t,t,t,t,t,a,t,t,a],
    [a,a,a,a,a,a,a,a,a,a,a,t,t,t,a,a]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

jogo5 :: Jogo 
jogo5 = Jogo {mapaJogo = mapa5, 
              inimigosJogo = [], 
              portaisJogo = [portal1_5, portal2_5, portal3_5], 
              torresJogo = [], 
              baseJogo = base5,
              lojaJogo = loja,
              acGeraCaminhos = 0}

base5 = baseTds {posicaoBase = (12,15)}

portal1_5 :: Portal
portal1_5 = Portal {posicaoPortal = (0,0),
                   ondasPortal = geraOndasPortal 2 3 1 (0,0)}

portal2_5 :: Portal
portal2_5 = Portal {posicaoPortal = (0,10),
                   ondasPortal = geraOndasPortal 3 2 1 (0,10)}

portal3_5 :: Portal
portal3_5 = Portal {posicaoPortal = (11,0),
                   ondasPortal = geraOndasPortal 3 3 2 (11,0)}

-- Jogo tutorial
jogoTT :: Jogo
jogoTT = Jogo {mapaJogo = mapa1,
                     inimigosJogo = [], 
                     portaisJogo = [portal6_1], 
                     torresJogo = [], 
                     baseJogo = base1, 
                     lojaJogo = loja,
                     acGeraCaminhos = 0}

portal6_1 :: Portal
portal6_1 = Portal {posicaoPortal = (5,0), 
                    ondasPortal = geraOndasPortal 1 3 2 (5,0)}