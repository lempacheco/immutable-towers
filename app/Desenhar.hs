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
desenha it = Pictures [picMapa,picInimigo,Pictures picPortais, picLoja, picBase, picTorre]
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
          picLoja = desenhaLoja loja texturas
          loja = lojaJogo jogo


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
        numeroDaVida = translate (x) (y+30) $ scale 0.1 0.1 $ text $ show $ vidaInimigo inimigo 
        textura = case tipoInimigo inimigo of
            MulherLanca   -> texturas !! 9
            GuerreiroFogo -> texturas !! 8
    in Pictures [translate x y textura, numeroDaVida]

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
    
desenhaLoja :: Loja -> [Picture] -> Picture
desenhaLoja loja ts = Pictures $ map desenhaTorre loja
    where x = -800
          y = 100
          espacamento = 200
          tamanhoTorre = 0.75
          tamanhoCreditos = 0.2
          desenhaTorre :: (Creditos, Torre) -> Picture
          desenhaTorre (cs,t) = case tipoProjetil $ projetilTorre t of
            Gelo -> Pictures [translate x y $ scale tamanhoTorre tamanhoTorre (ts!!3), translate (x+50) (y) $ scale tamanhoCreditos tamanhoCreditos $ text $ show cs, translate (x+135) (y+10) (ts!!14)]
            Resina -> Pictures [translate x (y-espacamento) $ scale tamanhoTorre tamanhoTorre (ts!!4), translate (x+50) (y-espacamento) $ scale tamanhoCreditos tamanhoCreditos $ text $ show cs, translate (x+150) (y-espacamento+14) (ts!!14)]
            Fogo -> Pictures [translate x (y-2*espacamento) $ scale tamanhoTorre tamanhoTorre (ts!!5), translate (x+50) (y-2*espacamento) $ scale tamanhoCreditos tamanhoCreditos $ text $ show cs, translate (x+150) (y-2*espacamento+14) (ts!!14)]
    
-- translate (-960+16*10) (540-16*10) $ scale 10 10 (ts!!10) -- painel
-- translate (x) (y+30) $ scale 0.1 0.1 $ text $ show $ vidaInimigo inimigo

desenhaMenu :: [Picture] -> Picture 
desenhaMenu texturas = pictures [fundo,
    translate 0 0 $ botaoPlay, 
    translate (0) (-2) $ botaoCredito, 
    translate  (0) (-4) $ botaoLevel
         ]
     where fundo = texturas !! 15 
           botaoPlay = texturas !! 16
           botaoCredito = texturas !! 17
           botaoLevel = texturas !! 18