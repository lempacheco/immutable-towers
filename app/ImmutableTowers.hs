module ImmutableTowers where

import LI12425
import Graphics.Gloss
import Tarefa2
import Tarefa3 

type Textura = (String,Picture)

data ImmutableTowers = ImmutableTowers {
    estadoIT :: EstadoJogo,
    estadoIT2 :: EstadoJogo,
    jogoIT :: Jogo,
    texturasIT :: [Textura], 
    posicaoSelecionadaMapa :: (Float, Float),
    posicaoSelecionadaMapaSndJog :: (Float, Float),
    produtoLoja :: (Float, Float),
    produtoLoja2 :: (Float, Float),
    botaoMenu :: (Float, Float),
    jogoItInicial :: Jogo,
    listaTerreno :: [(Posicao,Terreno)],
    listaPortais ::  [Portal], 
    baseCriada :: Bool,
    escolhendoParametros :: (Int, Int, Int),
    nivelJogoFinito :: NivelJogoFinito,
    nivelJogoInfinito :: Int, 
    botaoNivelPassado :: Posicao,
    botaoGameOver :: Posicao,
    modoJogo :: ModoJogo,
    etapaTT :: Int, 
    selecaoCostumizar :: Posicao,
    inimigoHomem :: String,
    inimigoMulher :: String,
    perfil :: String
}

data EstadoJogo = Menu 
                | Pausado 
                | VoltandoMenu
                | Jogando 
                | EscolhendoTorre
                | EscolhendoTorre2
                | EscolhendoTorre3
                | Comprando 
                | Comprando2
                | Comprando3
                | CriandoMapa
                | EscolhendoOndas 
                | EscolhendoIG
                | EscolhendoIM
                | GameOver
                | YouWon 
                | YouWon1
                | NivelPassado
                | Tutorial
                | TutorialEscolhendoTorre
                | TutorialComprando
                | MensagemErro
                | Costumizar
                deriving (Eq, Show)

data NivelJogoFinito = Nivel1 | Nivel2 | Nivel3 | Nivel4 | Nivel5 deriving (Eq, Show)

data ModoJogo = Finito | Infinito | MapaCriado deriving (Eq, Show)


{-| Controla a progressão de nível no jogo. 

-}

progredirNivel :: ImmutableTowers -> ImmutableTowers
progredirNivel it 
    | estadoIT it == NivelPassado || estadoIT it == YouWon = 
        case modoJogo it of 
            Finito   -> progredirNivelFinito it 
            Infinito -> progredirNivelInfinito it 
            _        -> it 
    | otherwise = it 

{-| Reinicia o nível atual que o jogador de encontra. 

-}

reiniciarNivel :: ImmutableTowers -> ImmutableTowers
reiniciarNivel it 
    | estadoIT it == NivelPassado || estadoIT it == GameOver || estadoIT it == YouWon || estadoIT it == YouWon1 || estadoIT it == Pausado = 
        case modoJogo it of 
            Finito   -> reiniciarNivelFinito it
            Infinito -> it {nivelJogoInfinito = nivelJogoInfinito it, estadoIT = Jogando, jogoIT = jogoItInicial it }
            _        -> it {jogoIT = jogoItInicial it, estadoIT = Jogando}
    | otherwise = it 

{-| Controla a progressão de nível no modo infinito, aumentando a dificuldade, sempre que o nível aumenta. 

-}

progredirNivelInfinito :: ImmutableTowers -> ImmutableTowers
progredirNivelInfinito it =  it {jogoIT = j {portaisJogo = pps, 
                                             baseJogo = novaBase, 
                                             torresJogo = []}, 
                                 estadoIT = Jogando, 
                                 nivelJogoInfinito = n} 
                          
   where n = nivelJogoInfinito it + 1 
         pps = aumentarDificuldadePortais n $ geraOndasInf n ps
         ps = portaisJogo j 
         novaBase = (baseJogo j) {creditosBase = creditosBase (baseJogo j) + 100,
                                  vidaBase = vidaBase (baseJogo j) + 500}
         j = jogoIT it 

{-| Aumenta a quantidade de ondas, e de inimigos em cada onda, nos portais do jogo no modo infinito.

-}

geraOndasInf :: Int -> [Portal] -> [Portal]
geraOndasInf _ [] = []
geraOndasInf n (p:ps) = p { ondasPortal = geraOndasPortal (1+n) (1+n) n (posicaoPortal p) } : geraOndasInf n ps 

{-| Ajusta a dificuldade dos portais do jogo. 

-}

aumentarDificuldadePortais :: Int -> [Portal] -> [Portal]
aumentarDificuldadePortais n ps = map (aumentarDificuldadePortal n) ps 

{-| Ajusta a dificuldade de um portal.

-}

aumentarDificuldadePortal :: Int -> Portal -> Portal 
aumentarDificuldadePortal n p = p {ondasPortal = map (aumentarDificuldadeOnda n) (ondasPortal p)}

{-| Aumenta a dificuldade de uma onda. 

-}

aumentarDificuldadeOnda :: Int -> Onda -> Onda 
aumentarDificuldadeOnda n o = o {inimigosOnda = map (aumentarDificuldadeInimigo n) (inimigosOnda o)}

{-| Aumenta a dificuldade de um inimigo. -}

aumentarDificuldadeInimigo :: Int -> Inimigo -> Inimigo 
aumentarDificuldadeInimigo n i = i {vidaInimigo = vidaInimigo i * fromIntegral n, 
                                    velocidadeInimigo = velocidadeInimigo i + fromIntegral n/2,
                                    ataqueInimigo = ataqueInimigo i + fromIntegral n/2}

{-| Controla a progressão dos níveis no modo finito.

-}

progredirNivelFinito :: ImmutableTowers -> ImmutableTowers
progredirNivelFinito it = if estadoIT it == NivelPassado || estadoIT it == YouWon then avancaNivelFinito it 
                          else if estadoIT it == GameOver || estadoIT it == YouWon || 
                                  estadoIT it == NivelPassado || estadoIT it == YouWon1 || 
                                  estadoIT it == Pausado
                          then reiniciarNivelFinito it 
                          else it  

{-| Responsável por avançar para o próximo nível no modo finito.

-}

avancaNivelFinito :: ImmutableTowers -> ImmutableTowers 
avancaNivelFinito it  = 
    case nivelJogoFinito it of 
        Nivel1 -> it {nivelJogoFinito = Nivel2, estadoIT = Jogando, jogoIT = jogo2}
        Nivel2 -> it {nivelJogoFinito = Nivel3, estadoIT = Jogando, jogoIT = jogo3}
        Nivel3 -> it {nivelJogoFinito = Nivel4, estadoIT = Jogando, jogoIT = jogo4}
        Nivel4 -> it {nivelJogoFinito = Nivel5, estadoIT = Jogando, jogoIT = jogo5}
        Nivel5 -> it {nivelJogoFinito = Nivel1, estadoIT = Jogando, jogoIT = jogo1}

{-| Responsável por reiniciar o nível que o jogador se encontrava.

-}

reiniciarNivelFinito :: ImmutableTowers -> ImmutableTowers 
reiniciarNivelFinito it  =
         case nivelJogoFinito it of 
            Nivel1 -> it {nivelJogoFinito = Nivel1, estadoIT = Jogando, jogoIT = jogo1}
            Nivel2 -> it {nivelJogoFinito = Nivel2, estadoIT = Jogando, jogoIT = jogo2}
            Nivel3 -> it {nivelJogoFinito = Nivel3, estadoIT = Jogando, jogoIT = jogo3}
            Nivel4 -> it {nivelJogoFinito = Nivel4, estadoIT = Jogando, jogoIT = jogo4}
            Nivel5 -> it {nivelJogoFinito = Nivel5, estadoIT = Jogando, jogoIT = jogo5}
