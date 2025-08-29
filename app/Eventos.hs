module Eventos where
    
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Event
import ImmutableTowers
import LI12425
import Tarefa1 
import Tarefa3
import FuncoesAux
import ElementosDoJogo

{-| Responsável por atualizar o jogo, sempre que o jogador interaja com o teclado. 

-}

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey KeyShiftL) Down _ _) it 
    | estadoIT it == Jogando = reiniciarEstado it
    | estadoIT it == Pausado = reiniciarEstado it
    | estadoIT it == CriandoMapa = reiniciarEstado it
    | otherwise = it

reageEventos (EventKey (Char 'r') Down _ _) it
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoSelecionadaMapa it
          listaTerrenoNova = atualizaMapa ((xF,yF), Relva) (listaTerreno it)  
      in it {listaTerreno = listaTerrenoNova}
    | otherwise = it 

reageEventos (EventKey (Char 't') Down _ _) it
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoSelecionadaMapa it
          listaTerrenoNova = atualizaMapa ((xF,yF), Terra) (listaTerreno it)  
      in it {listaTerreno = listaTerrenoNova}

reageEventos (EventKey (Char 'a') Down _ _) it
    | estadoIT it == CriandoMapa = 
      let (xF,yF) = posicaoSelecionadaMapa it
          listaTerrenoNova = atualizaMapa ((xF,yF), Agua) (listaTerreno it)  
      in it {listaTerreno = listaTerrenoNova}

reageEventos (EventKey (Char 'p') Down _ _) it 
    | estadoIT it == CriandoMapa = 
       it {estadoIT = EscolhendoOndas, 
           escolhendoParametros = (0,0,0)}

reageEventos (EventKey (SpecialKey KeyDelete) Down _ _ ) it
    | estadoIT it == CriandoMapa = 
      let (xF, yF) = posicaoSelecionadaMapa it
          j = jogoIT it 
          base = baseJogo j  
          apagaBase = if baseCriada it == True && posicaoBase base == (xF, yF) then False else baseCriada it 
      in it {listaPortais = deletePortal (listaPortais it) (xF,yF), baseCriada = apagaBase}
    | otherwise = it

reageEventos (EventKey (Char 'b') Down _ _) it 
    | estadoIT it == CriandoMapa && not (baseCriada it) = 
      let (xF,yF) = posicaoSelecionadaMapa it 
          lta = listaTerreno it 
          base = Base {vidaBase = 100,
                       posicaoBase = (xF, yF),
                       creditosBase = 1000}
      in if lookup (xF, yF) lta == Just Terra then it {jogoIT = j {baseJogo = base}, baseCriada = True}
         else it 
     where j = jogoIT it 

reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it 
    | estadoIT it == Menu && botaoMenu it == (-160,0) = if validaJogo $ jogoIT it 
                                                        then it {estadoIT = Jogando,  
                                                                 modoJogo = Finito} 
                                                        else it {estadoIT = MensagemErro, 
                                                                 estadoIT2 = MensagemErro, 
                                                                 modoJogo = Finito}
    | estadoIT it == Menu && botaoMenu it == (-160,-100) = if validaJogo $ jogoIT it 
                                                           then it {estadoIT = Jogando, 
                                                                    modoJogo = Infinito} 
                                                           else it {estadoIT = MensagemErro, 
                                                                    estadoIT2= MensagemErro, 
                                                                    modoJogo = Infinito}
    | estadoIT it == Menu && botaoMenu it == (-160,-200) = it {estadoIT = CriandoMapa, 
                                                               modoJogo = MapaCriado, 
                                                               jogoIT = (jogoIT it) {baseJogo = (baseJogo (jogoIT it)) {creditosBase = 1000} }} 
    | estadoIT it == Menu && botaoMenu it == (-160,-300) = it {estadoIT = Tutorial, 
                                                               etapaTT = 0}    
    | estadoIT it == Menu && botaoMenu it == (-160,-400) = it {estadoIT = Costumizar}    

    | estadoIT it == Jogando = it {estadoIT = EscolhendoTorre}
    | estadoIT it == EscolhendoTorre = it {estadoIT = Comprando}
    | estadoIT it == Comprando =  
        let jogo = jogoIT it
            (t,c) = colocaTorre (produtoLoja it) (posicaoSelecionadaMapa it)
            jogoAtualizado = compraTorre t c jogo
         in it {jogoIT = jogoAtualizado, 
                estadoIT = Jogando}

    | (estadoIT it == EscolhendoOndas) || (estadoIT it == EscolhendoIG) || (estadoIT it == EscolhendoIM) = 
      let (xF,yF) = posicaoSelecionadaMapa it 
          listaTerrenoNova = listaTerreno it 
          (nO, n1, n2) = escolhendoParametros it 
          portal = Portal 
                 {posicaoPortal = (xF,yF),
                  ondasPortal = geraOndasPortal nO n1 n2 (xF,yF)}
          novosPortais = adicionarPortais portal listaTerrenoNova (listaPortais it)
      in it {listaPortais = novosPortais, 
             estadoIT = CriandoMapa, 
             escolhendoParametros = (0,0,0)}

    | estadoIT it == CriandoMapa =
              let jogoAtual = (jogoIT it) {mapaJogo = mapaCriado, 
                                           portaisJogo = listaPortais it, 
                                           torresJogo = [], 
                                           inimigosJogo = []}
                  mapaCriado = transformaMapa (listaTerreno it)
                  parametrosAtualizados = escolhendoParametros it
              in if validaJogo jogoAtual
                  then it {jogoIT = jogoAtual, 
                           jogoItInicial = jogoAtual, 
                           escolhendoParametros = parametrosAtualizados, 
                           modoJogo = MapaCriado, 
                           estadoIT = Jogando}
                  else it {estadoIT = MensagemErro}
    | estadoIT it == MensagemErro && modoJogo it == MapaCriado = it {estadoIT = CriandoMapa}
    | estadoIT it == MensagemErro && (modoJogo it == Infinito || modoJogo it == Finito) = it {estadoIT = Menu} 

    | estadoIT it == NivelPassado && fst (botaoNivelPassado it) == -150 = progredirNivel it
    | estadoIT it == NivelPassado && fst (botaoNivelPassado it) == -500 = it {estadoIT = Menu, 
                                                                              jogoIT = jogoItInicial it}
    | estadoIT it == NivelPassado && fst (botaoNivelPassado it) == 200 = reiniciarNivel it 
    | estadoIT it == GameOver && fst (botaoGameOver it) == 100 = reiniciarNivel it 
    | (estadoIT it == GameOver || estadoIT it == Tutorial) && fst (botaoGameOver it) == -600 = reiniciarEstado it
    | estadoIT it == YouWonCM && fst (botaoGameOver it) == 100 = reiniciarNivel it 
    | estadoIT it == YouWonCM && fst (botaoGameOver it) == -600 = reiniciarEstado it
    | estadoIT it == YouWon && fst (botaoNivelPassado it) == -150 = progredirNivel it 
    | (estadoIT it == YouWon || estadoIT it == Pausado) && fst (botaoNivelPassado it) == -500 = reiniciarEstado it
    | (estadoIT it == YouWon || estadoIT it == Pausado) && fst (botaoNivelPassado it) == 200  = reiniciarNivel it  
    | estadoIT it == Pausado && fst (botaoNivelPassado it) == -150 =  it {estadoIT = Jogando}
    | estadoIT it == Pausado && fst (botaoNivelPassado it) == -500 = reiniciarEstado it 
    | estadoIT it == Pausado && fst (botaoNivelPassado it) == 200 = reiniciarNivel it 

    | estadoIT it == Tutorial && etapaTT it == 3 && fst (botaoGameOver it) == 100 = it {estadoIT = Tutorial, 
                                                                                        jogoIT = jogoTT, 
                                                                                        etapaTT = 4}
    | estadoIT it == Tutorial && etapaTT it == 4 = it {estadoIT = TutorialEscolhendoTorre, 
                                                       etapaTT = 5}
    | estadoIT it == TutorialEscolhendoTorre && etapaTT it == 5 = it {estadoIT = TutorialComprando, 
                                                                      etapaTT = 6} 
    | estadoIT it == TutorialComprando && etapaTT it == 6 = 
        let (t,c) = colocaTorre (produtoLoja it) (posicaoSelecionadaMapa it)
            jogoAtualizado = compraTorre  t c (jogoIT it)
        in it {estadoIT = Tutorial, 
               jogoIT = jogoAtualizado, 
               etapaTT = 7}
    | estadoIT it == Costumizar = alteraITCostumizar it
    | otherwise = it

reageEventos (EventKey (SpecialKey KeyShiftR) Down _ _)  it 
    | (estadoIT it == Jogando) || (estadoIT it == Comprando) || (estadoIT it == EscolhendoTorre) = it {estadoIT = Pausado}
    | estadoIT it == Pausado = it {estadoIT = Jogando}
    | otherwise = it

reageEventos (EventKey (SpecialKey KeyDown) Down _ _) it
    | estadoIT it == Menu && py > (-400)= it {botaoMenu = (px, py-100)}
    | (estadoIT it == EscolhendoTorre || estadoIT it == TutorialEscolhendoTorre) && b > (-300) = it {produtoLoja = (a, b - 200)}
    | (estadoIT it == Comprando || estadoIT it == TutorialComprando) && y < 15 = it {posicaoSelecionadaMapa = (x, y + 1)}
    | estadoIT it == CriandoMapa && y < 15 = it {posicaoSelecionadaMapa = (x, y + 1)}
    | estadoIT it == EscolhendoOndas && nO > 0 = it {escolhendoParametros = (nO - 1, n1, n2)} 
    | estadoIT it == EscolhendoIG && n1 > 0 = it {escolhendoParametros = (nO, n1 - 1, n2)}
    | estadoIT it == EscolhendoIM && n2 > 0 = it {escolhendoParametros = (nO, n1, n2 - 1)}
    | estadoIT it == Costumizar && ySelecaoCostumizar == 350 = it {selecaoCostumizar = (-400, -50)}
    | estadoIT it == Costumizar && ySelecaoCostumizar == -50 = it {selecaoCostumizar = (-600, -350)}
    | otherwise = it
  where (x, y) = posicaoSelecionadaMapa it
        (a, b) = produtoLoja it 
        (px, py) = botaoMenu it 
        (nO, n1, n2) = escolhendoParametros it
        (_, ySelecaoCostumizar) = selecaoCostumizar it 

reageEventos (EventKey (SpecialKey KeyRight) Down _ _) it
    | (estadoIT it == Comprando || estadoIT it == TutorialComprando) && x < 15 = it {posicaoSelecionadaMapa = (x + 1, y)}
    | estadoIT it == CriandoMapa && x < 15 = it {posicaoSelecionadaMapa = (x + 1, y)}
    | estadoIT it == EscolhendoOndas = it {estadoIT = EscolhendoIG}
    | estadoIT it == EscolhendoIG = it {estadoIT = EscolhendoIM}
    | (estadoIT it == GameOver || estadoIT it == Tutorial) && xBotaoGameOver == -600 = it {botaoGameOver = (100, yBotaoGameOver)}
    | estadoIT it == YouWonCM && xBotaoGameOver == -600 = it {botaoGameOver = (100, yBotaoGameOver)}
    | (estadoIT it == YouWon || estadoIT it == Pausado || estadoIT it == NivelPassado) && xBotaoNivelPassado < 200  = it {botaoNivelPassado = (xBotaoNivelPassado + 350, yBotaoNivelPassado)}
    | estadoIT it == Costumizar && (xSelecaoCostumizar < 200 && ySelecaoCostumizar > -350) = it {selecaoCostumizar = (xSelecaoCostumizar + 600, ySelecaoCostumizar)}
    | estadoIT it == Costumizar && (xSelecaoCostumizar < 200 && ySelecaoCostumizar == -350) = it {selecaoCostumizar = (xSelecaoCostumizar + 500, ySelecaoCostumizar)}
  where (x, y) = posicaoSelecionadaMapa it
        (xBotaoNivelPassado, yBotaoNivelPassado) = botaoNivelPassado it
        (xBotaoGameOver, yBotaoGameOver) = botaoGameOver it
        (xSelecaoCostumizar, ySelecaoCostumizar) = selecaoCostumizar it


reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) it
    | (estadoIT it == Comprando || estadoIT it == TutorialComprando) && x > 0 = it {posicaoSelecionadaMapa = (x - 1, y)}
    | estadoIT it == CriandoMapa && x > 0 = it {posicaoSelecionadaMapa = (x - 1, y)}
    | estadoIT it == EscolhendoIG = it {estadoIT = EscolhendoOndas}
    | estadoIT it == EscolhendoIM = it {estadoIT = EscolhendoIG}
    | (estadoIT it == GameOver || estadoIT it == Tutorial) && xBotaoGameOver == 100 = it {botaoGameOver = (-600, yBotaoGameOver)}
    | estadoIT it == YouWonCM && xBotaoGameOver == 100 = it {botaoGameOver = (-600, yBotaoGameOver)}
    | (estadoIT it == YouWon || estadoIT it == Pausado || estadoIT it == NivelPassado) && xBotaoNivelPassado > -500  = it {botaoNivelPassado = (xBotaoNivelPassado - 350, yBotaoNivelPassado)}
    | estadoIT it == Costumizar && (xSelecaoCostumizar > -400 && ySelecaoCostumizar > -350) = it {selecaoCostumizar = (xSelecaoCostumizar - 600, ySelecaoCostumizar)}
    | estadoIT it == Costumizar && (xSelecaoCostumizar > -400 && ySelecaoCostumizar == -350) = it {selecaoCostumizar = (xSelecaoCostumizar - 500, ySelecaoCostumizar)}
  where (x, y) = posicaoSelecionadaMapa it
        (xBotaoNivelPassado, yBotaoNivelPassado) = botaoNivelPassado it
        (xBotaoGameOver, yBotaoGameOver) = botaoGameOver it
        (xSelecaoCostumizar, ySelecaoCostumizar) = selecaoCostumizar it
    
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) it
    | estadoIT it == Menu && py < 0 = it {botaoMenu = (px, py+100)}
    | (estadoIT it == EscolhendoTorre || estadoIT it == TutorialEscolhendoTorre) && b < 100 = it {produtoLoja = (a, b + 200)}
    | (estadoIT it == Comprando || estadoIT it == TutorialComprando) && y > 0  = it {posicaoSelecionadaMapa = (x, y - 1)}
    | estadoIT it == CriandoMapa && y > 0 = it {posicaoSelecionadaMapa = (x, y - 1)}
    | estadoIT it == EscolhendoOndas = it {escolhendoParametros = (nO + 1, n1, n2)} 
    | estadoIT it == EscolhendoIG = it {escolhendoParametros = (nO, n1 + 1, n2)}
    | estadoIT it == EscolhendoIM = it {escolhendoParametros = (nO, n1, n2 + 1)}
    | estadoIT it == Costumizar && ySelecaoCostumizar == -50 = it {selecaoCostumizar = (-400, 350)}
    | estadoIT it == Costumizar && ySelecaoCostumizar == -350 = it {selecaoCostumizar = (-400, -50)}
  where (x, y) = posicaoSelecionadaMapa it
        (a, b) = produtoLoja it 
        (px, py) = botaoMenu it 
        (nO, n1, n2) = escolhendoParametros it
        (_, ySelecaoCostumizar) = selecaoCostumizar it 

reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it 
    | (estadoIT it == EscolhendoTorre || estadoIT it == Comprando) = it {estadoIT = Jogando}
    | estadoIT it == Tutorial && etapaTT it == 0 = it {estadoIT = Tutorial, etapaTT = 1}
    | estadoIT it == Tutorial && etapaTT it == 1 = it {estadoIT = Tutorial, etapaTT = 2}
    | estadoIT it == Tutorial && etapaTT it == 2 = it {estadoIT = Tutorial, etapaTT = 3}
    | estadoIT it == Tutorial && etapaTT it == 7 = reiniciarEstado it 
    | estadoIT it == Costumizar = it {estadoIT = Menu}

reageEventos (EventKey (SpecialKey KeyCtrlL) Down _ _) it 
    | (estadoIT2 it == EscolhendoTorre2 || estadoIT2 it == Comprando2) = it {estadoIT2 = Jogando}

reageEventos (EventKey (SpecialKey KeyTab) Down _ _) it 
    | estadoIT2 it == Jogando = it {estadoIT2 = EscolhendoTorre2}
    | estadoIT2 it == EscolhendoTorre2 = it {estadoIT2 = Comprando2}
    | estadoIT2 it == Comprando2 =  
        let jogo = jogoIT it
            (t,c) = colocaTorre (produtoLoja2 it) (posicaoSelecionadaMapaSndJog it)
            jogoAtualizado = compraTorre t c jogo
         in it {jogoIT = jogoAtualizado, 
                estadoIT2 = Jogando}


reageEventos (EventKey (Char 'w') Down _ _) it
    | estadoIT2 it == EscolhendoTorre2 && b < 100 = it {produtoLoja2 = (a, b + 200)}
    | estadoIT2 it == Comprando2 && y > 0 = it {posicaoSelecionadaMapaSndJog = (x, y - 1)}
  where (a,b) = produtoLoja2 it 
        (x,y) = posicaoSelecionadaMapaSndJog it 

reageEventos (EventKey (Char 's') Down _ _) it
    | estadoIT2 it == EscolhendoTorre2  && b > (-300) = it {produtoLoja2 = (a, b - 200)}
    | estadoIT2 it == Comprando2 && y < 15 = it {posicaoSelecionadaMapaSndJog = (x, y + 1)}
  where (a,b) = produtoLoja2 it 
        (x,y) = posicaoSelecionadaMapaSndJog it 

reageEventos (EventKey (Char 'a') Down _ _) it
   | estadoIT2 it == Comprando2 && x > 0 = it {posicaoSelecionadaMapaSndJog = (x - 1, y)}
  where (x,y) = posicaoSelecionadaMapaSndJog it 

reageEventos (EventKey (Char 'd') Down _ _) it
   | estadoIT2 it == Comprando2 && x < 15 = it {posicaoSelecionadaMapaSndJog = (x + 1, y)}
  where (x,y) = posicaoSelecionadaMapaSndJog it 

reageEventos (EventKey (Char 'm') Down _ _) it  
   | not (multiplayer it) = ativaMP (it {multiplayer = True})
   | multiplayer it = ativaMP (it {multiplayer = False})

reageEventos _ it = it 

{-| Costumiza o perfil do jogador e os inimigos, com base na posição da seta. 

-}

alteraITCostumizar :: ImmutableTowers -> ImmutableTowers
alteraITCostumizar it = case selecaoCostumizar it of
    (-400, 350) -> it {inimigoHomem = "guerreiro"}
    (200, 350) -> it {inimigoHomem = "viking"}
    (-400, -50) -> it {inimigoMulher = "mulherLanca"}
    (200, -50) -> it {inimigoMulher = "guerreiraMulher"}
    (-600, -350) -> it {perfil = "perfilGuerreiro"}
    (-100, -350) -> it {perfil = "perfilViking"}
    _ -> it {perfil = "perfilMulherLanca"}

{-| Responsável por posicionar uma torre comprada no mapa. 

-}

colocaTorre :: Posicao -> Posicao -> (Torre, Creditos)
colocaTorre prodLoja (xF, yF) = case prodLoja of
    (_, 100) -> (Torre { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                            danoTorre = 15,
                            alcanceTorre = 4,
                            rajadaTorre = 4,
                            cicloTorre = 5*60,
                            tempoTorre = 5*60,
                            projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita (2*60)},
                            iteracoesDesdeInicioAnimacao = 1}, 100)

    (_, -100) -> (Torre { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                            danoTorre = 25,
                            alcanceTorre = 4,
                            rajadaTorre = 3,
                            cicloTorre = 4*60,
                            tempoTorre = 4*60,
                            projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita},
                            iteracoesDesdeInicioAnimacao = 1}, 150)

    _ -> (Torre { posicaoTorre = (xF,yF), -- sincroniza posição da torre com a seleção
                  danoTorre = 30,
                  alcanceTorre = 3,
                  rajadaTorre = 5,
                  cicloTorre = 4*60,
                  tempoTorre = 4*60,
                  projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita (3*60)},
                  iteracoesDesdeInicioAnimacao = 1}, 200)

{-| Estado inicial do jogo, que é aplicado sempre que o jogador volta oa menu. 

-}

reiniciarEstado :: ImmutableTowers -> ImmutableTowers
reiniciarEstado it = it {
                         estadoIT = Menu, 
                         jogoIT = jogo1,
                         posicaoSelecionadaMapa = (0,0),
                         produtoLoja = (-900, 100),
                         botaoMenu = (-160, 0),
                         jogoItInicial = jogo1, 
                         listaTerreno = [], 
                         listaPortais = [],
                         escolhendoParametros = (0,0,0),
                         baseCriada = False,
                         nivelJogoFinito = Nivel1,
                         nivelJogoInfinito = 1, 
                         botaoNivelPassado = (-500, -250),
                         botaoGameOver = (-600, -250),
                         etapaTT = 0 
                        }