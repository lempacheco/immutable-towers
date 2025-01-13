module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Tarefa2 

{-| Responsável por atualizar o jogo com o passar do tempo, de acordo com o framerate.-}

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it  
    | estadoIT it == Menu = it
    | estadoIT it == Pausado = it
    | estadoIT it == CriandoMapa = it 
    | estadoIT it == EscolhendoOndas = it
    | estadoIT it == EscolhendoIG = it
    | estadoIT it == EscolhendoIM = it 
    | estadoIT it == NivelPassado = it
    | estadoIT it == Tutorial && etapaTT it < 3 = it
    | (perdeuJogo j || ganhouJogo j) && (estadoIT it == Tutorial || estadoIT it == TutorialEscolhendoTorre || estadoIT it == TutorialComprando) = it
    | ganhouJogo j && modoJogo it == MapaCriado = it {estadoIT = YouWonCM}
    | ganhouJogo j && nivelJogoFinito it /= Nivel5 = it {estadoIT = NivelPassado}
    | ganhouJogo j && nivelJogoFinito it == Nivel5 = it {estadoIT = YouWon}
    | perdeuJogo j = it {estadoIT = GameOver} 
    | estadoIT it == MensagemErro = it
    | estadoIT it == Costumizar = it
    | otherwise = it {jogoIT = atualizaJogo t $ j}
    where j = jogoIT it
 
