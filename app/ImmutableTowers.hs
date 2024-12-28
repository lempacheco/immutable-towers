module ImmutableTowers where
import LI12425
import Graphics.Gloss

type Texturas = [(Terreno, (Picture, (Float,Float)))] --correspondencia entre um Terreno e a sua textura respetiva, juntamente com as suas coordenadas

data ImmutableTowers = ImmutableTowers {
    jogoIT :: Jogo,
    texturasIT :: Texturas
}
