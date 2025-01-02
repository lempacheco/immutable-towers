
module Nivel1 where

import LI12425
import ImmutableTowers

it1 :: [Textura] -> ImmutableTowers
it1 texturas = 
    ImmutableTowers {estadoIT = Menu, 
                     jogoIT = Jogo {baseJogo = base,
                                    torresJogo = [torre1,torre2,torre3],
                                    portaisJogo = [portal1,portal2],
                                    mapaJogo = mapa,
                                    inimigosJogo = [],
                                    lojaJogo = loja},
                     texturasIT = texturas, 
                     posicaoTorreComprada = (0.5,0.5)}

base :: Base
base = Base {vidaBase = 50,
             posicaoBase = (7.5*64,-(1.5*64)),
             creditosBase = 1000}

torre1 :: Torre
torre1 = Torre {posicaoTorre = (-5.5*64, -5.5*64), 
                projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}, 
                danoTorre = 5,
                alcanceTorre = 5*64,
                rajadaTorre = 3,
                cicloTorre = 180,
                tempoTorre = 180,
                iteracoesDesdeInicioAnimacao = 1}

torre2 :: Torre
torre2 = Torre {posicaoTorre = (-4.5*64, 4.5*64), 
                projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, 
                danoTorre = 3,
                alcanceTorre = 5*64,
                rajadaTorre = 3,
                cicloTorre = 180,
                tempoTorre = 180,
                iteracoesDesdeInicioAnimacao = 1}

torre3 :: Torre
torre3 = Torre {posicaoTorre = (6.5*64, 0.5*64), 
                projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}, 
                danoTorre = 1,
                alcanceTorre = 5*64,
                rajadaTorre = 3,
                cicloTorre = 180,
                tempoTorre = 180,
                iteracoesDesdeInicioAnimacao = 1}

portal1 :: Portal
portal1 = Portal {posicaoPortal = (-(7.5*64),-(1.5*64)),
                  ondasPortal = [Onda {inimigosOnda = [inimigo1, inimigo2],
                  cicloOnda = 2*60,
                  tempoOnda = 0,
                  entradaOnda = 0}]}

portal2 :: Portal
portal2 = Portal {posicaoPortal = (-(2.5*64),7.5*64), 
                  ondasPortal = [Onda {inimigosOnda = [inimigo3,inimigo4],
                  cicloOnda = 2*60,
                  tempoOnda = 0,
                  entradaOnda = 0}]}

inimigo1 :: Inimigo
inimigo1 = Inimigo {posicaoInimigo = (-(7.5*64),-(1.5*64)), 
                    tipoInimigo = GuerreiroFogo, 
                    projeteisInimigo = [], 
                    vidaInimigo = 1000, 
                    butimInimigo = 4, 
                    direcaoInimigo = Este, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 70, 
                    caminhoInimigo = [],
                    acDirecao = (-(7.5*64),-(1.5*64))}

inimigo2 :: Inimigo
inimigo2 = Inimigo {posicaoInimigo = (-(7.5*64),-(1.5*64)), 
                    tipoInimigo = MulherLanca, 
                    projeteisInimigo = [], 
                    vidaInimigo = 6, 
                    butimInimigo = 4, 
                    direcaoInimigo = Este, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 70,
                    caminhoInimigo = [],
                    acDirecao = (-(7.5*64),-(1.5*64))}

inimigo3 :: Inimigo
inimigo3 = Inimigo {posicaoInimigo = (-(2.5*64),7.5*64), 
                    tipoInimigo = GuerreiroFogo, 
                    projeteisInimigo = [], 
                    vidaInimigo = 1000, 
                    butimInimigo = 4, 
                    direcaoInimigo = Norte, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 70, 
                    caminhoInimigo = [],
                    acDirecao = (-(2.5*64),7.5*64)}

inimigo4 :: Inimigo
inimigo4 = Inimigo {posicaoInimigo = (-(2.5*64),7.5*64), 
                    tipoInimigo = MulherLanca, 
                    projeteisInimigo = [], 
                    vidaInimigo = 6, 
                    butimInimigo = 4, 
                    direcaoInimigo = Norte, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 70,
                    caminhoInimigo = [],
                    acDirecao = (-(2.5*64),7.5*64)}

mapa :: Mapa 
mapa = 
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
       
loja = [(1000, Torre{projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 10}}),
        (1000, Torre{projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}}),
        (1000, Torre{projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}})
        ]