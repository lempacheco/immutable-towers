module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 

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

reageEventos (EventKey (Char 'w') Down _ _) it 
    | estadoIT it == CriandoMapa && y > 0 = it {posicaoTorreComprada = (x, y - 1)}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (Char 'd') Down _ _) it
    | estadoIT it == CriandoMapa && x < 15 = it {posicaoTorreComprada = (x + 1, y)}
  where (x, y) = posicaoTorreComprada it

reageEventos (EventKey (Char 'a') Down _ _) it
    | estadoIT it == CriandoMapa && x > 0 = it {posicaoTorreComprada = (x - 1, y)}
  where (x, y) = posicaoTorreComprada it
    
reageEventos (EventKey (Char 's') Down _ _) it
    | estadoIT it == CriandoMapa && y < 15 = it {posicaoTorreComprada = (x, y + 1)}
  where (x, y) = posicaoTorreComprada it

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
      let (xF,yF) = posicaoTorreComprada it 
          listaTerrenoNova = listaTerreno it 
          portal = Portal 
                 {posicaoPortal = (xF,yF),
                  ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (xF,yF), 
                                                                tipoInimigo = GuerreiroFogo, 
                                                                projeteisInimigo = [], 
                                                                vidaInimigo = 1000, 
                                                                butimInimigo = 4,  
                                                                ataqueInimigo = 5, 
                                                                velocidadeInimigo = 2, 
                                                                caminhoInimigo = [],
                                                                acDirecao = (xF,yF),
                                                                iteracoesDesdeInicioAnimacaoInimigo = 1}
                                                      ],
                  cicloOnda = 2*60,
                  tempoOnda = 0,
                  entradaOnda = 0}]}
          novosPortais = adicionarPortais portal listaTerrenoNova (listaPortais it)
      in it {listaPortais = novosPortais}

reageEventos (EventKey (Char 'o') Down _ _) it 
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoTorreComprada it 
          base = Base {vidaBase = 50,
                       posicaoBase = (xF,yF),
                       creditosBase = 1000}
      in it {jogoIT = (jogoIT it) {baseJogo = base}}


reageEventos (EventKey (SpecialKey (KeyEnter)) Down _ _) it 
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
    | estadoIT it == CriandoMapa = 
         it {estadoIT = Jogando, jogoIT = jogoAtual}
           where jogoAtual = (jogoIT it) {mapaJogo = mapaCriado, portaisJogo = listaPortais it, torresJogo = [], inimigosJogo = []}
                 mapaCriado = transformaMapa (listaTerreno it)
    
reageEventos (EventKey (Char 'b') Down _ _)  it 
    | estadoIT it == Jogando = it {estadoIT = Pausado}
    | estadoIT it == Pausado = it {estadoIT = Jogando}
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
