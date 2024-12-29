module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

l :: Integer
l = 64

altura :: Integer
altura = 64*16

comprimento :: Integer
comprimento = 64*16

desenha :: ImmutableTowers -> Picture
desenha it = Pictures [picMapa,picInimigo]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          texturas = texturasIT it
         -- picBase = desenhaBase base (textures !! 4)
         -- base = baseJogo (jogoIT it)
          picInimigo = desenhaInimigos inimigos (texturas !! 3)
          inimigos = inimigosJogo jogo


desenhaMapa :: Mapa -> [Picture] -> Picture
desenhaMapa mapa textures =
    let t = getMapaTexturas mapa textures
    in pictures [translate ((fromInteger x * fromInteger l )-7.5*64) ((fromInteger y * fromInteger l) +7.5*64 ) ((t!!abs(fromInteger y))!!fromInteger x) | (x,y) <- positions]

selectTexture :: [Picture] -> Terreno -> Picture
selectTexture textures Terra = head textures
selectTexture textures Relva = textures !! 1
selectTexture textures Agua = textures !! 2

positions :: [(Integer,Integer)]
positions = [(x,y) | y <- [0,(-1)..(-15)], x <- [0..15]]

getMapaTexturas :: Mapa -> [Picture] -> [[Picture]]
getMapaTexturas [] _ = []
getMapaTexturas (h:t) textures = map (textures `selectTexture`) h : getMapaTexturas t textures


desenhaBase :: Base -> Picture -> Picture
desenhaBase base textura =
    let (x,y) = posicaoBase base
    in translate x y textura  

desenhaInimigos :: [Inimigo] -> Picture -> Picture
desenhaInimigos inimigos textura = pictures [translate (x * 64) (y * 64) textura | Inimigo {posicaoInimigo = (x, y)} <- inimigos]



desenhaPortais :: [Portal] -> Picture
desenhaPortais (p:ps) = undefined