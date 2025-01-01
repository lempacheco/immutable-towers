module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Tarefa1

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it  
    | estadoIT it == Menu = it 
    | otherwise = it {jogoIT = atualizaJogo t $ jogoIT it}
