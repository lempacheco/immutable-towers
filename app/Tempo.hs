module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it  
    | estadoIT it == Menu = it
    | estadoIT it == Pausado = it
    | estadoIT it == NivelPassado = it
    | (vidaBase $ baseJogo $ jogoIT it) <= 0 = it {estadoIT = NivelPassado}
    | otherwise = it {jogoIT = atualizaJogo t $ jogoIT it}
