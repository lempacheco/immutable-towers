module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it  
    | estadoIT it == Menu = it
    | estadoIT it == Pausado = it
    | estadoIT it == CriandoMapa = it 
    | estadoIT it == EscolhendoOndas = it
    | estadoIT it == EscolhendoIG = it
    | estadoIT it == EscolhendoIM = it 
    | estadoIT it == NivelPassado = it
    | length iTotal == 0 && modoDeJogo it /= Nivel5 && modoDeJogo it /= MapaCriadoJogador = it {estadoIT = NivelPassado}
    | vidaBase b <= 0 = it {estadoIT = GameOver}
    | otherwise = it {jogoIT = atualizaJogo t $ j}
    where j = jogoIT it
          ps = portaisJogo $ j
          os = concat (map ondasPortal ps)
          is = concat (map inimigosOnda os)
          iAtivos = inimigosJogo $ j
          iTotal = is ++ iAtivos
          b = baseJogo j
