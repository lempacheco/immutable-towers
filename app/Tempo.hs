module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3

import Tarefa1 

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it = it {jogoIT = atualizaJogo t $ jogoIT it}

geraUmcaminho :: Mapa -> Inimigo -> Base -> [Posicao]
geraUmcaminho m i b = 
    let posI = posicaoInimigo i 
        posB = posicaoBase b
    in snd $ atualizaPos m posI posB []
