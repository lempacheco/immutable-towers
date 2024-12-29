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
desenha it = Pictures [picMapa,picInimigo,Pictures picPortais, picBase, picTorre]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          texturas = texturasIT it
          picBase = desenhaBase base (texturas !! 6)
          base = baseJogo jogo
          picInimigo = desenhaInimigos inimigos texturas
          inimigos = inimigosJogo jogo
          picPortais = desenhaPortais portais (texturas !! 7)
          portais = portaisJogo jogo
          picTorre = desenhaTorres torres texturas 
          torres = torresJogo jogo


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

{- desenhaInimigos :: [Inimigo] -> Picture -> Picture
desenhaInimigos inimigos textura = pictures [translate (x * 64) (y * 64) textura | Inimigo {posicaoInimigo = (x, y)} <- inimigos] -}

desenhaInimigos :: [Inimigo] -> [Picture] -> Picture
desenhaInimigos inimigos texturas = Pictures $ map (`desenhaUmInimigo` texturas) inimigos 
    

desenhaUmInimigo :: Inimigo -> [Picture] -> Picture 
desenhaUmInimigo inimigo texturas = 
    let (x, y) = posicaoInimigo inimigo
        textura = case tipoInimigo inimigo of
            MulherLanca   -> texturas !! 9
            GuerreiroFogo -> texturas !! 8
    in translate x y textura

desenhaTorres :: [Torre] -> [Picture] -> Picture 
desenhaTorres torres texturas = Pictures $ map (`desenhaUmaTorre` texturas) torres 

desenhaUmaTorre :: Torre -> [Picture] -> Picture
desenhaUmaTorre torre texturas = 
    let (x,y) = posicaoTorre torre 
        textura = case tipoProjetil (projetilTorre torre) of 
            Gelo -> texturas !! 3
            Resina -> texturas !! 4 
            Fogo -> texturas !! 5
    in translate x y textura 

desenhaPortais :: [Portal] -> Picture -> [Picture]
desenhaPortais [] _ = []
desenhaPortais (p:ps) textura = 
    let (x,y) = posicaoPortal p
    in translate x (y  + (128-64)/2) textura : desenhaPortais ps textura
