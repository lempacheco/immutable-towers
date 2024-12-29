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
desenha it = Pictures [picMapa,picInimigo,Pictures picPortais, picBase]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          texturas = texturasIT it
          picBase = desenhaBase base (texturas !! 6)
          base = baseJogo jogo
          picInimigo = desenhaInimigos inimigos (texturas !! 3)
          inimigos = inimigosJogo jogo
          picPortais = desenhaPortais portais (texturas !! 7)
          portais = portaisJogo jogo


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
    in translate x (y  + (104-64)/2) textura  

desenhaInimigos :: [Inimigo] -> Picture -> Picture
desenhaInimigos inimigos textura = pictures [translate (x * 64) (y * 64) textura | Inimigo {posicaoInimigo = (x, y)} <- inimigos]

desenhaPortais :: [Portal] -> Picture -> [Picture]
desenhaPortais [] _ = []
desenhaPortais (p:ps) textura = 
    let (x,y) = posicaoPortal p
    in translate x (y  + (128-64)/2) textura : desenhaPortais ps textura