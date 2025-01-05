module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 
import Tarefa3
import Data.List (sortOn)

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it 
    | estadoIT it == Menu = it {estadoIT = Jogando} 
    | estadoIT it == Jogando = it {estadoIT = Menu, jogoIT = jogoItInicial it}
    | estadoIT it == Pausado = it {estadoIT = Menu, jogoIT = jogoItInicial it}
    | otherwise = it

reageEventos (EventKey (Char 'r') Down _ _) it
    | estadoIT it == Menu = it {estadoIT = CriandoMapa}
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoTorreComprada it
          listaTerrenoNova = atualizaMapa ((xF,yF), Relva) (listaTerreno it)  
      in it {listaTerreno = listaTerrenoNova}
    | otherwise = it 

reageEventos (EventKey (Char 't') Down _ _) it
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoTorreComprada it
          listaTerrenoNova = atualizaMapa ((xF,yF), Terra) (listaTerreno it)  
      in it {listaTerreno = listaTerrenoNova}

reageEventos (EventKey (Char 'y') Down _ _) it
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoTorreComprada it
          listaTerrenoNova = atualizaMapa ((xF,yF), Agua) (listaTerreno it)  
      in it {listaTerreno = listaTerrenoNova}

reageEventos (EventKey (Char 'p') Down _ _) it 
    | estadoIT it == CriandoMapa = 
       it {estadoIT = EscolhendoOndas, escolhendoParametros = (0,0,0)}

reageEventos (EventKey (Char 'o') Down _ _) it 
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoTorreComprada it 
      in it {jogoIT = (jogoIT it) {baseJogo = baseTds {posicaoBase = (xF,yF)}}}


reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it 
    | estadoIT it == Jogando = it {estadoIT = EscolhendoTorre}
    | estadoIT it == EscolhendoTorre = it {estadoIT = Comprando, produtoLoja = produtoLoja it}
    | estadoIT it == Comprando =  
        let (xF,yF) = posicaoTorreComprada it -- posição da seleção vermelha
            jogo = jogoIT it
            (torre, custoTorre) = 
              case produtoLoja it of
                (-900, 100) -> (Torre { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                                        danoTorre = 15,
                                        alcanceTorre = 5,
                                        rajadaTorre = 4,
                                        cicloTorre = 5*60,
                                        tempoTorre = 5*60,
                                        projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita (2*60)},
                                        iteracoesDesdeInicioAnimacao = 1}, 100)

                (-900, -100) -> (Torre { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                                          danoTorre = 25,
                                          alcanceTorre = 3,
                                          rajadaTorre = 3,
                                          cicloTorre = 4*60,
                                          tempoTorre = 4*60,
                                          projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                                          iteracoesDesdeInicioAnimacao = 1}, 150)

                _ -> (Torre { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                              danoTorre = 30,
                              alcanceTorre = 4,
                              rajadaTorre = 5,
                              cicloTorre = 4*60,
                              tempoTorre = 4*60,
                              projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita (3*60)},
                              iteracoesDesdeInicioAnimacao = 1}, 200)
            jogoAtualizado = compraTorre torre custoTorre jogo
         in it {jogoIT = jogoAtualizado, estadoIT = Jogando}

    | estadoIT it == EscolhendoOndas || estadoIT it == EscolhendoIG || estadoIT it == EscolhendoIM = 
      let (xF,yF) = posicaoTorreComprada it 
          listaTerrenoNova = listaTerreno it 
          (nO, n1, n2) = escolhendoParametros it 
          portal = Portal 
                 {posicaoPortal = (xF,yF),
                  ondasPortal = geraOndasPortal nO n1 n2 (xF,yF)}
          novosPortais = adicionarPortais portal listaTerrenoNova (listaPortais it)
      in it {listaPortais = novosPortais, estadoIT = CriandoMapa, escolhendoParametros = (0,0,0)}
  
reageEventos (EventKey (Char 'v') Down _ _) it 
    | estadoIT it == CriandoMapa =
       it {estadoIT = Jogando, jogoIT = jogoAtual, escolhendoParametros = parametrosAtualizados }
            where jogoAtual = (jogoIT it) {mapaJogo = mapaCriado, portaisJogo = listaPortais it, torresJogo = [], inimigosJogo = [], baseJogo = baseTds {posicaoBase = (xF,yF)} }
                  mapaCriado = transformaMapa (listaTerreno it)
                  parametrosAtualizados = escolhendoParametros it
                  (xF,yF) = posicaoTorreComprada it 
reageEventos (EventKey (Char 'b') Down _ _)  it 
    | estadoIT it == Jogando = it {estadoIT = Pausado}
    | estadoIT it == Pausado = it {estadoIT = Jogando}
    | otherwise = it

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) it
    | estadoIT it == EscolhendoTorre && b > (-300) = it {produtoLoja = (a, b - 200)}
    | estadoIT it == Comprando && y < 15 = it {posicaoTorreComprada = (x, y + 1)}
    | estadoIT it == CriandoMapa && y < 15 = it {posicaoTorreComprada = (x, y + 1)}
    | estadoIT it == EscolhendoOndas && nO > 0 = it {escolhendoParametros = (nO - 1, n1, n2)} 
    | estadoIT it == EscolhendoIG && n1 > 0 = it {escolhendoParametros = (nO, n1 - 1, n2)}
    | estadoIT it == EscolhendoIM && n2 > 0 = it {escolhendoParametros = (nO, n1, n2 - 1)}
    | otherwise = it 
  where (x, y) = posicaoTorreComprada it
        (a, b) = produtoLoja it 
        (nO, n1, n2) = escolhendoParametros it 

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) it
    | estadoIT it == Comprando && x < 15 = it {posicaoTorreComprada = (x + 1, y)}
    | estadoIT it == CriandoMapa && x < 15 = it {posicaoTorreComprada = (x + 1, y)}
    | estadoIT it == EscolhendoOndas = it {estadoIT = EscolhendoIG}
    | estadoIT it == EscolhendoIG = it {estadoIT = EscolhendoIM}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) it
    | estadoIT it == Comprando && x > 0 = it {posicaoTorreComprada = (x - 1, y)}
    | estadoIT it == CriandoMapa && x > 0 = it {posicaoTorreComprada = (x - 1, y)}
    | estadoIT it == EscolhendoIG = it {estadoIT = EscolhendoOndas}
    | estadoIT it == EscolhendoIM = it {estadoIT = EscolhendoIG}
  where (x, y) = posicaoTorreComprada it
    
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) it
    | estadoIT it == EscolhendoTorre && b < 100 = it {produtoLoja = (a, b + 200)}
    | estadoIT it == Comprando && y > 0  = it {posicaoTorreComprada = (x, y - 1)}
    | estadoIT it == CriandoMapa && y > 0 = it {posicaoTorreComprada = (x, y - 1)}
    | estadoIT it == EscolhendoOndas = it {escolhendoParametros = (nO + 1, n1, n2)} 
    | estadoIT it == EscolhendoIG = it {escolhendoParametros = (nO, n1 + 1, n2)}
    | estadoIT it == EscolhendoIM = it {escolhendoParametros = (nO, n1, n2 + 1)}
  where (x, y) = posicaoTorreComprada it
        (a, b) = produtoLoja it 
        (nO, n1, n2) = escolhendoParametros it 

reageEventos _ it = it 

compraTorre :: Torre -> Creditos -> Jogo -> Jogo
compraTorre t custoTorre j 
    | custoTorre <= creditosBase (baseJogo j) && validaTorre j {torresJogo = t:torresJogo j} = jogoNovo
    | otherwise = j 
  where jogoNovo = j {baseJogo = (baseJogo j) {creditosBase = creditosBase (baseJogo j) - custoTorre}, 
                      torresJogo = t:torresJogo j}

-- adiciona terreno na lista 
atualizaMapa :: (Posicao, Terreno) -> [(Posicao,Terreno)] -> [(Posicao, Terreno)]
atualizaMapa (pos, ter) [] = [(pos,ter)]
atualizaMapa (pos, ter) lt = if (pos, ter) `elem` lt then lt else (pos,ter) : lt

transformaMapa :: [(Posicao, Terreno)] -> Mapa
transformaMapa listaTerreno =
    [[procuraTerrenoNaLista (x, y) listaTerreno | x <- [0..15]] | y <- [0..15]]
  where
    procuraTerrenoNaLista :: Posicao -> [(Posicao, Terreno)] -> Terreno
    procuraTerrenoNaLista pos lt =
        case lookup pos lt of
            Just terreno -> terreno
            Nothing -> Relva -- se não for defindio

adicionarPortais :: Portal -> [(Posicao, Terreno)] -> [Portal] -> [Portal]
adicionarPortais p lt ps
    | lookup pp lt == Just Terra = p : ps
    | otherwise = ps
  where
    pp = posicaoPortal p
