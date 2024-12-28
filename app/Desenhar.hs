module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Data.Maybe

l :: Float
l = 64

altura :: Integer
altura = 1080

comprimento :: Integer
comprimento = 1920

desenha :: ImmutableTowers -> Picture
desenha it = Pictures desenhoMapa
    where desenhoMapa = desenhaMapa (fromInteger comprimento) (fromInteger altura) mapa textures
          mapa = mapaJogo (jogoIT it)
          textures = texturasIT it

desenhaMapa :: Float -> Float -> Mapa -> Texturas -> [Picture]
desenhaMapa _ _ [] _ = []
desenhaMapa x y (h:t) texturas = linha ++ resto
    where
        linha = desenhaLinha x y h texturas
        resto = desenhaMapa x (y-l) t texturas

desenhaLinha :: Float -> Float -> [Terreno] -> Texturas -> [Picture]
desenhaLinha _ _ [] _ = []
desenhaLinha x y (h:t) texturas = terreno : resto
    where
        terreno = desenhaTerreno x y h texturas
        resto = desenhaLinha (x+l) y t texturas
 
desenhaTerreno :: Float -> Float -> Terreno -> Texturas -> Picture
desenhaTerreno x y terreno texturas = Translate realX realY textura
    where 
        tuple = fromJust $ lookup terreno texturas
        textura = fst tuple
        realX = ((+x) . fst . snd) tuple
        realY = ((+y) . snd . snd) tuple

{- lookup' :: (Eq a) => a -> [(a,b)] -> b
lookup' n ((x,y):t)
    | n == x = y
    | otherwise = lookup' n t -}