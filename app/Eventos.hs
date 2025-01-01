module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it 
    | estadoIT it == Menu = it {estadoIT = Jogando} 
    | otherwise = it

reageEventos (EventKey (SpecialKey (KeyEnter)) Down _ _) it 
    | estadoIT it == Jogando = it {estadoIT = Comprando}
    | otherwise = it  

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) it
    | estadoIT it == Comprando && y > -7 = it {posicaoTorreComprada = (x, y - 1)}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) it
    | estadoIT it == Comprando && x < 7 = it {posicaoTorreComprada = (x + 1, y)}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) it
    | estadoIT it == Comprando && x > -7 = it {posicaoTorreComprada = (x - 1, y)}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (SpecialKey KeyUp) Down _ _) it
    | estadoIT it == Comprando && y < 7 = it {posicaoTorreComprada = (x, y + 1)}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (Char 'g') Down _ _) it
    | estadoIT it == Comprando =
        let posFinal@(xF,yF) = posicaoTorreComprada it -- posição da seleção vermelha
            jogo = jogoIT it
            torre = Torre
              { posicaoTorre = (xF*64,yF*64), -- sincroniza posição da torre com a seleção
                danoTorre = 10,
                alcanceTorre = 5*64,
                rajadaTorre = 2,
                cicloTorre = 180,
                tempoTorre = 180,
                projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                iteracoesDesdeInicioAnimacao = 1
              }
            custoTorre = 50
            jogoAtualizado = compraTorre torre custoTorre $ colocaTorreNaPos torre posFinal jogo
         in it {jogoIT = jogoAtualizado, estadoIT = Jogando}

reageEventos _ it = it 

compraTorre :: Torre -> Creditos ->  Jogo -> Jogo
compraTorre t custoTorre j 
    | custoTorre <= creditosBase (baseJogo j) = jogoNovo
    | otherwise = j 
  where jogoNovo = j {baseJogo = (baseJogo j) {creditosBase = creditosBase (baseJogo j) - custoTorre}, 
                      torresJogo = t:torresJogo j}

colocaTorreNaPos :: Torre -> Posicao -> Jogo -> Jogo
colocaTorreNaPos t posM j = if posicaoValida posM m then j {torresJogo = t:torresJogo j} 
                            else j 
   where m = mapaJogo j 
         
posicaoValida :: Posicao -> Mapa -> Bool
posicaoValida (x, y) mapa =
    case procuraTerreno ((x / 64), (y / 64)) mapa of
        Just Relva -> True
        _          -> False

