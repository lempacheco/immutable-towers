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
        inimigosJogo = [],
        lojaJogo = []
      },
  texturasIT = texturas
}

mapaInicial :: Mapa
mapaInicial =
 [ [t, t, r, a, a, a],
   [r, t, r, a, r, r],                     
   [r, t, r, a, r, t],
   [r, t, r, a, t, t],
   [r, t, t, t, t, t],
   [a, a, a, a, r, r]
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

{- vazia :: Picture
vazia = Color black (Polygon [(0,0),(l,0),(l,l),(0,l),(0,0)]) -}

main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"
  terra <- loadBMP "resources/textures/map/Terra.bmp"
  agua <- loadBMP "resources/textures/map/Agua.bmp"
  relva <- loadBMP "resources/textures/map/Relva.bmp"
  inimigo1 <- loadBMP "resources/textures/entities/Inimigo1.bmp"
  play janela 
        fundo 
        fr 
        (itInicial 
          [
            terra,
            relva,
            agua,
            inimigo1
          ]
        ) 
        desenha 
        reageEventos 
        reageTempo