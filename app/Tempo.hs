module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it  
    | estadoIT it == Menu = it
    | estadoIT it == Pausado = it
    | otherwise = it {jogoIT = atualizaJogo t $ jogoIT it}
