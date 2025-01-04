module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it 
    | estadoIT it == Menu = it {estadoIT = Jogando} 
    | estadoIT it == Jogando = it {estadoIT = Menu}
    | estadoIT it == Pausado = it {estadoIT = Menu}
    | otherwise = it

reageEventos (EventKey (SpecialKey (KeyEnter)) Down _ _) it 
    | estadoIT it == Jogando = it {estadoIT = EscolhendoTorre}
    | estadoIT it == EscolhendoTorre = it {estadoIT = Comprando, produtoLoja = produtoLoja it}
    | estadoIT it == Comprando =  case produtoLoja it of 
      (-900, 100) -> 
        let (xF,yF) = posicaoTorreComprada it -- posição da seleção vermelha
            jogo = jogoIT it 
            torre = Torre
              { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                danoTorre = 10,
                alcanceTorre = 5*64,
                rajadaTorre = 2,
                cicloTorre = 180,
                tempoTorre = 180,
                projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                iteracoesDesdeInicioAnimacao = 1
              }
            custoTorre = 50
            jogoAtualizado = compraTorre torre custoTorre jogo
         in it {jogoIT = jogoAtualizado, estadoIT = Jogando}
      (-900, -100) -> 
        let (xF,yF) = posicaoTorreComprada it -- posição da seleção vermelha
            jogo = jogoIT it 
            torre = Torre
              { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                danoTorre = 10,
                alcanceTorre = 5*64,
                rajadaTorre = 2,
                cicloTorre = 180,
                tempoTorre = 180,
                projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                iteracoesDesdeInicioAnimacao = 1
              }
            custoTorre = 50
            jogoAtualizado = compraTorre torre custoTorre jogo
         in it {jogoIT = jogoAtualizado, estadoIT = Jogando}
      (-900, -300) -> 
        let (xF,yF) = posicaoTorreComprada it -- posição da seleção vermelha
            jogo = jogoIT it 
            torre = Torre
              { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                danoTorre = 10,
                alcanceTorre = 5*64,
                rajadaTorre = 2,
                cicloTorre = 180,
                tempoTorre = 180,
                projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3},
                iteracoesDesdeInicioAnimacao = 1
              }
            custoTorre = 50
            jogoAtualizado = compraTorre torre custoTorre jogo
         in it {jogoIT = jogoAtualizado, estadoIT = Jogando}
    | otherwise = it  

reageEventos (EventKey (Char 'b') Down _ _)  it 
    | estadoIT it == Jogando = it {estadoIT = Pausado}
    | estadoIT it == Pausado = it {estadoIT = Jogando}
    | otherwise = it

reageEventos (EventKey (Char 'm') Down _ _)  it 
    | estadoIT it == Pausado = it {estadoIT = Menu, jogoIT = jogoItInicial it}
    | otherwise = it

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) it
    | estadoIT it == EscolhendoTorre && b > (-300) = it {produtoLoja = (a, b - 200)}
    | estadoIT it == Comprando && y < 15 = it {posicaoTorreComprada = (x, y + 1)}
  where (x, y) = posicaoTorreComprada it
        (a, b) = produtoLoja it 

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) it
    | estadoIT it == Comprando && x < 15 = it {posicaoTorreComprada = (x + 1, y)}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) it
    | estadoIT it == Comprando && x > 0 = it {posicaoTorreComprada = (x - 1, y)}
  where (x, y) = posicaoTorreComprada it
    
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) it
    | estadoIT it == EscolhendoTorre && b < 100 = it {produtoLoja = (a, b + 200)}
    | estadoIT it == Comprando && y > 0  = it {posicaoTorreComprada = (x, y - 1)}
  where (x, y) = posicaoTorreComprada it
        (a, b) = produtoLoja it 

reageEventos (EventKey (Char 'g') Down _ _) it
    | estadoIT it == Comprando =
        let (xF,yF) = posicaoTorreComprada it -- posição da seleção vermelha
            jogo = jogoIT it
            torre = Torre
              { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                danoTorre = 10,
                alcanceTorre = 5*64,
                rajadaTorre = 2,
                cicloTorre = 180,
                tempoTorre = 180,
                projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3},
                iteracoesDesdeInicioAnimacao = 1
              }
            custoTorre = 50
            jogoAtualizado = compraTorre torre custoTorre jogo
         in it {jogoIT = jogoAtualizado, estadoIT = Jogando}

reageEventos _ it = it 

compraTorre :: Torre -> Creditos -> Jogo -> Jogo
compraTorre t custoTorre j 
    | custoTorre <= creditosBase (baseJogo j) && validaTorre j {torresJogo = t:torresJogo j} = jogoNovo
    | otherwise = j 
  where jogoNovo = j {baseJogo = (baseJogo j) {creditosBase = creditosBase (baseJogo j) - custoTorre}, 
                      torresJogo = t:torresJogo j}
