module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

l :: Float
l = 64

altura :: Integer
altura = 64*18

comprimento :: Integer
comprimento = 64*18

desenha :: ImmutableTowers -> Picture
desenha it = pictures [picMapa, picBase, picInimigos]
    where picMapa = desenhaMapa mapa textures
          mapa = mapaJogo (jogoIT it)
          textures = texturasIT it
        --  picBase = desenhaBase base -- indice  (textures) 
        --  base = baseJogo (JogoIT it)
       

{- desenhaMapa :: Float -> Float -> Mapa -> Texturas -> [Picture]
desenhaMapa _ _ [] _ = []
desenhaMapa x y (h:t) texturas = linha ++ resto
    where
        linha = desenhaLinha x y h texturas
        resto = desenhaMapa x (y-l) t texturas -}

desenhaMapa :: Mapa -> [Picture] -> Picture
desenhaMapa mapa textures =
    pictures [translate x y t | ((x,y),t) <- zip positions (map selectTexture (concat mapa))]
        where
            selectTexture :: Terreno -> Picture
            selectTexture Terra = head textures
            selectTexture Relva = textures !! 1
            selectTexture Agua = textures !! 2
            positions = [(x*l,y*l) | y <- [-9..9], x <- [-9..9]]

{- desenhaInimigo :: Inimigo -> [Picture] -> Picture
desenhaInimigo  -}

desenhaBase :: Base -> Picture -> Picture
desenhaBase base textura =
    let (x,y) = posicaoBase base
    in translate x y textura 

desenhaInimigo :: Inimigo -> Picture -> Picture 
desenhaInimigo inimigo textura = 
    let (x,y) = posicaoInimigo inimigo
    in translate x y textura


desenhaPortais :: [Portal] -> Picture
desenhaPortais (p:ps) = undefined