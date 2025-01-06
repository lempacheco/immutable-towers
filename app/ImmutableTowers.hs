module ImmutableTowers where

import LI12425
import Graphics.Gloss

type Textura = (String,Picture)

data ImmutableTowers = ImmutableTowers {
    estadoIT :: EstadoJogo,
    jogoIT :: Jogo,
    texturasIT :: [Textura], 
    posicaoTorreComprada :: (Float, Float),
    produtoLoja :: (Float, Float),
    jogoItInicial :: Jogo,
    listaTerreno :: [(Posicao,Terreno)],
    listaPortais ::  [Portal], 
    escolhendoParametros :: (Int, Int, Int),
    modoDeJogo :: ModoJogo,
    botaoNivelPassado :: Posicao,
    botaoGameOver :: Posicao
}

data EstadoJogo = Menu 
                | Jogando 
                | EscolhendoTorre
                | Comprando 
                | CriandoMapa
                | EscolhendoOndas
                | EscolhendoIG
                | EscolhendoIM
                | Pausado 
                | VoltandoMenu
                | GameOver
                | YouWon 
                | NivelPassado
                deriving (Eq, Show)

data ModoJogo = Nivel1 | Nivel2 | Nivel3 | Nivel4 | Nivel5 | MapaCriadoJogador deriving (Eq, Show)