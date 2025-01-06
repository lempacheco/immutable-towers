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
    baseCriada :: Bool,
    escolhendoParametros :: (Int, Int, Int)
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
