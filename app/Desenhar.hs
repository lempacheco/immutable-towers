module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

l :: Integer
l = 64

altura :: Integer
altura = 64*18

comprimento :: Integer
comprimento = 64*18

--altura = 64*16

--comprimento :: Integer
--comprimento = 64*16

desenha :: ImmutableTowers -> Picture
desenha it = pictures [picMapa, picInimigo]
    where textures = texturasIT it
          picMapa = desenhaMapa mapa textures
          mapa = mapaJogo (jogoIT it)
         -- picBase = desenhaBase base (textures !! 4)
         -- base = baseJogo (jogoIT it)
         picInimigo = desenhaInimigos inimigos (textures !! 3)
         inimigos = inimigosJogo (jogoIT it)
 
       

{- desenhaMapa :: Float -> Float -> Mapa -> Texturas -> [Picture]
desenhaMapa _ _ [] _ = []
desenhaMapa x y (h:t) texturas = linha ++ resto
    where
        linha = desenhaLinha x y h texturas
        resto = desenhaMapa x (y-l) t texturas -}

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

{- desenhaInimigo :: Inimigo -> [Picture] -> Picture
desenhaInimigo  -}

{- desenhaBase :: Base -> Picture -> Picture
desenhaBase base textura =
    let (x,y) = posicaoBase base
    in translate x y textura  -}

desenhaInimigos :: [Inimigo] -> Picture -> Picture
desenhaInimigos inimigos textura = pictures [translate (fromInteger x * 64) (fromInteger y * 64) textura | Inimigo {posicaoInimigo = (x, y)} <- inimigos]



desenhaPortais :: [Portal] -> Picture
desenhaPortais (p:ps) = undefined