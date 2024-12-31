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

reageEventos (EventKey (SpecialKey (KeyDown)) Down _ _) it 
    | estadoIT it == Comprando = 
        let (x,y) = posicaoTorreComprada it  
        in it {posicaoTorreComprada = (x, y-1) }
    | otherwise = it 

reageEventos (EventKey (SpecialKey (KeyRight)) Down _ _) it 
    | estadoIT it == Comprando = 
        let (x,y) = posicaoTorreComprada it  
        in it {posicaoTorreComprada = (x+1, y) }
    | otherwise = it 

reageEventos (EventKey (SpecialKey (KeyLeft)) Down _ _) it 
    | estadoIT it == Comprando = 
        let (x,y) = posicaoTorreComprada it 
        in it {posicaoTorreComprada = (x-1, y) }
    | otherwise = it     
    
reageEventos (EventKey (SpecialKey (KeyUp)) Down _ _) it 
    | estadoIT it == Comprando = 
        let (x,y) = posicaoTorreComprada it 
        in it {posicaoTorreComprada = (x, y+1) }
    | otherwise = it 

reageEventos _ it = it 

compraTorre :: Torre -> Creditos ->  Jogo -> Jogo
compraTorre t custoTorre j 
    | custoTorre <= creditosBase (baseJogo j) = jogoNovo
    | otherwise = j 
  where jogoNovo = j {baseJogo = (baseJogo j) {creditosBase = creditosBase (baseJogo j) - custoTorre}, 
                      torresJogo = t:torresJogo j}

colocaTorre :: Torre -> Posicao -> Jogo -> Jogo
colocaTorre t posM j = if posicaoValida posM m then j {torresJogo = t:torresJogo j} else j 
   where m = mapaJogo j 

posicaoValida :: Posicao -> Mapa -> Bool
posicaoValida (x, y) mapa =
    case procuraTerreno ((x / 64), (y / 64)) mapa of
        Just Terra -> True
        _          -> False
