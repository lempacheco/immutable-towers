module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Data.Maybe (fromJust)
import GHC.Float (int2Float)

lado :: Integer
lado = 64

altura :: Integer
altura = 64*16

comprimento :: Integer
comprimento = 64*16

desenha :: ImmutableTowers -> Picture
desenha it = case estadoIT it of
     Menu -> desenhaMenu  it 
     Jogando -> desenhaJogo it 
     Comprando -> desenhaComprando it
     --ColocandoTorre -> desenhaTorreNova it

{- desenhaTorreNova :: ImmutableTowers -> Picture 
desenhaTorreNova it = Pictures [desenhaJogo it, desenhaUmaTorre torre texturas]
   where jogo = jogoIT it 
         torre = Torre { -- Exemplo de torre que pode ser comprada
                   posicaoTorre = posicaoTorreComprada it,
                   danoTorre = 10,
                   alcanceTorre = 3,
                   rajadaTorre = 2,
                   cicloTorre = 1,
                   tempoTorre = 0,
                   projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3}
                  } 
         texturas = texturasIT it  -}


desenhaComprando :: ImmutableTowers -> Picture 
desenhaComprando it = Pictures [desenhaJogo it, desenhaSelecao selec]
  where
    selec = posicaoTorreComprada it

-- Função para desenhar a seleção no mapa
desenhaSelecao :: (Float, Float) -> Picture
desenhaSelecao (x, y) =
    translate (x * 64) (y * 64) $
    color (withAlpha 0.5 red) $ rectangleSolid 64 64

desenhaMenu :: ImmutableTowers -> Picture 
desenhaMenu it = Pictures 
    [fundo,
    translate 0 0 $ botaoPlay, 
    translate (0) (-56) $ botaoCredito, 
    translate  (0) (-112) $ botaoLevel
         ]
     where texturas = texturasIT it 
           fundo = fromJust $ lookup "fundoMenu" texturas 
           botaoPlay = fromJust $ lookup "botaoPlay" texturas
           botaoCredito = fromJust $ lookup "botaoCredito" texturas
           botaoLevel = fromJust $ lookup "botaoLevel" texturas

desenhaJogo :: ImmutableTowers -> Picture
desenhaJogo it = Pictures [picMapa,picInimigo,Pictures picPortais, picLoja, picBase, picTorre]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          texturas = texturasIT it
          picBase = desenhaBase base (fromJust $ lookup "base" texturas)
          base = baseJogo jogo
          picInimigo = desenhaInimigos inimigos texturas
          inimigos = inimigosJogo jogo
          picPortais = desenhaPortais portais (fromJust $ lookup "portal" texturas)
          portais = portaisJogo jogo
          picTorre = desenhaTorres torres texturas 
          torres = torresJogo jogo
          picLoja = desenhaLoja loja texturas
          loja = lojaJogo jogo


desenhaMapa :: Mapa -> [Textura] -> Picture
desenhaMapa mapa textures =
    let t = getMapaTexturas mapa textures
    in Pictures [translate 0 0 (fromJust $ lookup "fundoJogo" textures), pictures [translate ((fromInteger x * fromInteger lado )-7.5*64) ((fromInteger y * fromInteger lado) +7.5*64 ) ((t!!abs (fromInteger y))!!fromInteger x) | (x,y) <- positions]]

selectTexture :: [Textura] -> Terreno -> Picture
selectTexture textures Terra = fromJust $ lookup "terra" textures
selectTexture textures Relva = fromJust $ lookup "relva" textures
selectTexture textures Agua = fromJust $ lookup "agua" textures

positions :: [(Integer,Integer)]
positions = [(x,y) | y <- [0,(-1)..(-15)], x <- [0..15]]

getMapaTexturas :: Mapa -> [Textura] -> [[Picture]]
getMapaTexturas [] _ = []
getMapaTexturas (h:t) textures = map (textures `selectTexture`) h : getMapaTexturas t textures


desenhaBase :: Base -> Picture -> Picture
desenhaBase base textura =
    let (x,y) = posicaoBase base
    in translate x (y  + (104-64)/2) textura  

{- desenhaInimigos :: [Inimigo] -> Picture -> Picture
desenhaInimigos inimigos textura = pictures [translate (x * 64) (y * 64) textura | Inimigo {posicaoInimigo = (x, y)} <- inimigos] -}

string2FonteNumeros :: String -> [Textura] -> Picture
string2FonteNumeros s ts = Pictures $ auxString2FonteNumeros s ts 0

auxString2FonteNumeros :: String -> [Textura] -> Float -> [Picture]
auxString2FonteNumeros [] _ _ = []
auxString2FonteNumeros (h:t) ts ac = (translate (ac*espacamento) 0 $ fromJust $ lookup ("numero" ++ [h]) ts) : auxString2FonteNumeros t ts (ac+1)
    where espacamento = 13

desenhaInimigos :: [Inimigo] -> [Textura] -> Picture
desenhaInimigos inimigos texturas = Pictures $ map (`desenhaUmInimigo` texturas) inimigos 

desenhaUmInimigo :: Inimigo -> [Textura] -> Picture 
desenhaUmInimigo inimigo texturas = 
    let (x, y) = posicaoInimigo inimigo
        comprimentoNumeroVidaPxs = int2Float (length (show $ ceiling $ vidaInimigo inimigo) * 13)
        offsetNumeroVida = (comprimentoNumeroVidaPxs+27+18)/2*0.5 --metade do comprimento da vida, da largura do inimigo e da largura do coração, escalado a 0.5
        numeroDaVida = translate (x-offsetNumeroVida) (y+40) $ scale 0.5 0.5 $ string2FonteNumeros (show $ ceiling $ vidaInimigo inimigo) texturas
        coracaoVida = translate (x+offsetNumeroVida) (y+40-(16/2*0.7)) $ scale 0.7 0.7 $ fromJust $ lookup "vida" texturas
        textura = case tipoInimigo inimigo of
            MulherLanca   -> fromJust $ lookup "mulherLanca" texturas
            GuerreiroFogo -> fromJust $ lookup "guerreiroFogo" texturas
    in Pictures [translate x y textura, numeroDaVida, coracaoVida]

desenhaTorres :: [Torre] -> [Textura] -> Picture 
desenhaTorres torres texturas = Pictures $ map (`desenhaUmaTorre` texturas) torres 

desenhaUmaTorre :: Torre -> [Textura] -> Picture
desenhaUmaTorre torre texturas = 
    let (x,y) = posicaoTorre torre 
        textura = case tipoProjetil (projetilTorre torre) of 
            Gelo -> fromJust $ lookup "torreGelo" texturas
            Resina -> fromJust $ lookup "torreResina" texturas
            Fogo -> fromJust $ lookup "torreFogo" texturas
    in translate x y textura 

desenhaPortais :: [Portal] -> Picture -> [Picture]
desenhaPortais [] _ = []
desenhaPortais (p:ps) textura = 
    let (x,y) = posicaoPortal p
    in translate x (y  + (128-64)/2) textura : desenhaPortais ps textura
    
desenhaLoja :: Loja -> [Textura] -> Picture
desenhaLoja loja ts = Pictures $ map desenhaTorre loja
    where x = -800
          y = 100
          espacamento = 200
          tamanhoTorre = 0.75
          tamanhoCreditos = 0.2
          desenhaTorre :: (Creditos, Torre) -> Picture
          desenhaTorre (cs,t) = case tipoProjetil $ projetilTorre t of
            Gelo -> Pictures [translate x y $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreGelo" ts), translate (x+50) (y) $ scale tamanhoCreditos tamanhoCreditos $ text $ show cs, translate (x+135) (y+10) (fromJust $ lookup "creditos" ts)]
            Resina -> Pictures [translate x (y-espacamento) $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreResina" ts), translate (x+50) (y-espacamento) $ scale tamanhoCreditos tamanhoCreditos $ text $ show cs, translate (x+150) (y-espacamento+14) (fromJust $ lookup "creditos" ts)]
            Fogo -> Pictures [translate x (y-2*espacamento) $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreFogo" ts), translate (x+50) (y-2*espacamento) $ scale tamanhoCreditos tamanhoCreditos $ text $ show cs, translate (x+150) (y-2*espacamento+14) (fromJust $ lookup "creditos" ts)]




-- translate (-960+16*10) (540-16*10) $ scale 10 10 (ts!!10) -- painel
-- translate (x) (y+30) $ scale 0.1 0.1 $ text $ show $ vidaInimigo inimigo

