module ImmutableTowers where
import LI12425
import Graphics.Gloss

type Textura = (String,Picture)

data ImmutableTowers = ImmutableTowers {
    estadoIT :: EstadoJogo,
    jogoIT :: Jogo,
    texturasIT :: [Textura], 
    posicaoTorreComprada :: (Float, Float)
}

data EstadoJogo = Menu 
                | Jogando 
                | ColocandoTorre
                | Comprando 
                | Pausado 
                | GameOver
                | YouWon 
                deriving (Eq, Show)