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
    listaPortais ::  [Portal]
}

data EstadoJogo = Menu 
                | Jogando 
                | EscolhendoTorre
                | Comprando 
                | CriandoMapa
                | Pausado 
                | VoltandoMenu
                | GameOver
                | YouWon 
                deriving (Eq, Show)
