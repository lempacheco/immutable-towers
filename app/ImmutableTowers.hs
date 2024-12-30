module ImmutableTowers where
import LI12425
import Graphics.Gloss

type Textura = (String,Picture)

data ImmutableTowers = ImmutableTowers {
    estadoIT :: EstadoJogo,
    jogoIT :: Jogo,
    texturasIT :: [Textura]
}

data EstadoJogo = Menu 
                | Jogando 
                | Pausado 
                | GameOver
                | YouWon 
                deriving (Eq, Show)