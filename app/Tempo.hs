module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Tarefa2 

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it  
    | estadoIT it == Menu = it
    | estadoIT it == Pausado = it
    | estadoIT it == CriandoMapa = it 
    | estadoIT it == EscolhendoOndas = it
    | estadoIT it == EscolhendoIG = it
    | estadoIT it == EscolhendoIM = it 
    | estadoIT it == NivelPassado = it
    | ganhouJogo j && modoJogo it == MapaCriado = it {estadoIT = YouWon1}
    | ganhouJogo j && nivelJogoFinito it /= Nivel5 = it {estadoIT = NivelPassado}
    | ganhouJogo j && nivelJogoFinito it == Nivel5 = it {estadoIT = YouWon}
    | perdeuJogo j = it {estadoIT = GameOver} 
    | otherwise = it {jogoIT = atualizaJogo t $ j}
    where j = jogoIT it

