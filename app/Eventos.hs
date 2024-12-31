module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey (KeyDown)) Down _ _) it 
    | estadoIT it == Menu = it {estadoIT = Jogando} 
    | otherwise = it
reageEventos _ it = it 

{- 
{- reageColocaTorre :: Event -> ImmutableTowers -> ImmutableTowers  
reageColocaTorre (EventKey (Char 'g' )) Down _ (x,y) it 
    | estadoIT it == Jogando = it {jogoIT = jogoAtualizado}
  where jogo = jogoIT it 
        mapa = mapaJogo jogo
        torre = (torreJogo jogo) {posicaoTorre = (x,y)}
        custo = 50 
        base = baseJogo jogo
        creditoDaBase = creditosBase base
        jogoAtualizado = compraTorre torre custo $ colocaTorre torre (x,y) jogo  
reageColocaTorre _ it = it  -}
 
reageColocaTorre :: Event -> ImmutableTowers -> ImmutableTowers  
reageColocaTorre (EventKey (Char 'g') Down _ pos) it
    | estadoIT it == Jogando =
        let jogo = jogoIT it
            mapa = mapaJogo jogo
            base = baseJogo jogo
            torre = Torre {
                posicaoTorre = pos, -- Define a posição selecionada
                danoTorre = 10,     -- Exemplo de dano
                alcanceTorre = 5,   -- Exemplo de alcance
                rajadaTorre = 3,    -- Exemplo de rajada
                cicloTorre = 2,     -- Exemplo de ciclo
                tempoTorre = 0,     -- Exemplo de tempo inicial
                projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 5}
            }
            custo = 50 -- Exemplo de custo
            creditoDaBase = creditosBase base
            jogoAtualizado =
                if posicaoValida pos mapa && custo <= creditoDaBase
                then compraTorre torre custo $ colocaTorre torre pos jogo
                else jogo -- Não atualiza o jogo se a posição for inválida ou os créditos forem insuficientes
        in it { jogoIT = jogoAtualizado }
reageColocaTorre _ it = it -- Evento ignorado para estados diferentes ou outras teclas


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
  -}