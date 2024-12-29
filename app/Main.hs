module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425

itInicial :: [Picture] -> ImmutableTowers
itInicial texturas = ImmutableTowers { 
  jogoIT = Jogo {
        baseJogo = Base {
          vidaBase = 50,
          posicaoBase = (0,0),
          creditosBase = 0
          },
        portaisJogo = [],
        torresJogo = [],
        mapaJogo = mapaInicial,
        inimigosJogo = [inimigo1],
        lojaJogo = []
      },
  texturasIT = texturas
}

mapaInicial :: Mapa 
mapaInicial = 
  [ [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,t,t,t,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,r,r,t,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,r,r,t,r,r,r,a,a,r,r,r],
    [r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,r,r,r],
    [r,r,t,r,r,r,t,t,r,r,r,a,a,r,r,r],
    [r,r,t,r,r,r,t,r,r,r,a,a,a,r,r,r],
    [t,t,t,r,r,r,t,r,r,a,a,a,r,t,t,t],
    [r,r,r,r,r,r,t,r,r,a,a,r,r,t,r,r],
    [r,r,r,r,r,r,t,t,t,t,t,t,t,t,t,t],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

janela :: Display
janela = InWindow "Immutable Towers" (fromInteger comprimento, fromInteger altura) (0, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"
  terra <- loadBMP "resources/textures/map/Terra.bmp"
  agua <- loadBMP "resources/textures/map/Agua.bmp"
  relva <- loadBMP "resources/textures/map/Relva.bmp"
  torreGelo <- loadBMP "resources/textures/towers/TorreGelo.bmp"
  torreResina <- loadBMP "resources/textures/towers/TorreResina.bmp"
  torreFogo <- loadBMP "resources/textures/towers/TorreGelo.bmp"
  base <- loadBMP "resources/textures/base/Base.bmp"
  portal <- loadBMP "resources/textures/portal/Portal.bmp"
  guerreiroFogo <- loadBMP "resources/textures/entities/GuerreiroFogo.bmp"
  mulherLanca <- loadBMP "resources/textures/entities/MulherLanca.bmp"
  play janela 
        fundo 
        fr 
        (itInicial 
          [
            terra,         --0
            relva,         --1
            agua,          --2
            torreGelo,     --3
            torreResina,   --4
            torreFogo,     --5
            base,          --6
            portal,        --7
            guerreiroFogo, --8
            mulherLanca    --9
          ]
        ) 
        desenha 
        reageEventos 
        reageTempo