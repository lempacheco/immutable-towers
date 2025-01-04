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
     EscolhendoTorre -> Pictures [desenhaEscolhendoTorre it, Translate 0 0 $ scale 1 1 $ text $ show $ produtoLoja it] 
     Comprando -> desenhaComprando it
     Pausado -> Pictures [desenhaJogo it, desenhaPausa ts]  
  where ts = texturasIT it

desenhaComprando :: ImmutableTowers -> Picture 
desenhaComprando it = Pictures [desenhaJogo it, desenhaSelecao selec]
  where
    selec = posicaoTorreComprada it

desenhaEscolhendoTorre :: ImmutableTowers -> Picture 
desenhaEscolhendoTorre it = Pictures [desenhaJogo it, desenhaSelecaoLoja selec ts]
  where selec = produtoLoja it 
        ts = texturasIT it

desenhaSelecaoLoja :: (Float, Float) -> [Textura] -> Picture
desenhaSelecaoLoja (x,y) ts = Pictures [Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts]

-- Função para desenhar a seleção no mapa
desenhaSelecao :: (Float, Float) -> Picture
desenhaSelecao pos =
    let (x,y) = conversaoCoordsGloss pos
    in translate x y $
       color (withAlpha 0.5 red) $ rectangleSolid 64 64

desenhaMenu :: ImmutableTowers -> Picture 
desenhaMenu it = Pictures 
    [fundoTela, fundo,
    translate 0 0 $ botaoPlay, 
    translate (0) (-56) $ botaoCredito, 
    translate  (0) (-112) $ botaoLevel
         ]
     where texturas = texturasIT it 
           fundoTela = fromJust $ lookup "fundoJogo" texturas 
           fundo = fromJust $ lookup "fundoMenu" texturas 
           botaoPlay = fromJust $ lookup "botaoPlay" texturas
           botaoCredito = fromJust $ lookup "botaoCredito" texturas
           botaoLevel = fromJust $ lookup "botaoLevel" texturas

desenhaJogo :: ImmutableTowers -> Picture
desenhaJogo it = Pictures [picMapa, picMolduraMapa, picInimigo,Pictures picPortais, picLoja, picBase, picTorre, creditosJog]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          picMolduraMapa = desenhaMolduraMapa texturas
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
          creditosJog = desenhaPerfilJogador jogo base texturas 

desenhaMolduraMapa :: [Textura] -> Picture
desenhaMolduraMapa ts = translate 0 0 $ scale 1 1 $ (fromJust $ lookup "molduraMapa2" ts)


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

conversaoCoordsGloss :: Posicao -> Posicao
conversaoCoordsGloss (x,y) = ((x*64) - (7.5*64), (7.5*64) - (y*64))

desenhaBase :: Base -> Picture -> Picture
desenhaBase base textura =
    let (x,y) = conversaoCoordsGloss $ posicaoBase base
    in translate x (y  + (104-64)/2) textura  --desenho no mapa tendo em conta a altura da textura 

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
    let (x, y) = conversaoCoordsGloss $ posicaoInimigo inimigo
        comprimentoNumeroVidaPxs = int2Float (length (show $ ceiling $ vidaInimigo inimigo) * 13)
        offsetNumeroVida = (comprimentoNumeroVidaPxs+27+18)/2*0.5 --metade do comprimento da vida, da largura do inimigo e da largura do coração, escalado a 0.5
        numeroDaVida = translate (x-offsetNumeroVida) (y+40) $ scale 0.5 0.5 $ string2FonteNumeros (show $ ceiling $ vidaInimigo inimigo) texturas
        coracaoVida = translate (x+offsetNumeroVida) (y+40-(16/2*0.7)) $ scale 0.7 0.7 $ fromJust $ lookup "vida" texturas
        textura = desenhaAnimacaoInimigo inimigo texturas
    in Pictures [translate x y textura, numeroDaVida, coracaoVida, Translate x y $ scale 0.1 0.1 $ (text (show $ velocidadeInimigo inimigo))]

desenhaTorres :: [Torre] -> [Textura] -> Picture 
desenhaTorres torres texturas = Pictures $ map (`desenhaUmaTorre` texturas) torres 

desenhaUmaTorre :: Torre -> [Textura] -> Picture
desenhaUmaTorre torre texturas = 
    let (x,y) = conversaoCoordsGloss $ posicaoTorre torre 
        textura = case tipoProjetil (projetilTorre torre) of 
            Gelo -> fromJust $ lookup "torreGelo" texturas
            Resina -> fromJust $ lookup "torreResina" texturas
            Fogo -> fromJust $ lookup "torreFogo" texturas
        texturaAnimacao = desenhaAnimacaoTorre torre texturas
    in Pictures [translate x (y + (121-64)/2) textura, texturaAnimacao]

desenhaAnimacaoTorre :: Torre -> [Textura] -> Picture
desenhaAnimacaoTorre t ts =
    let (x,y) = conversaoCoordsGloss $ posicaoTorre t
        iteracoes = iteracoesDesdeInicioAnimacao t
        textura = case tipoProjetil (projetilTorre t) of
            Gelo -> fromJust $ lookup ("animacaoTorreGelo" ++ show iteracoes) ts
            Resina -> fromJust $ lookup ("animacaoTorreResina" ++ show iteracoes) ts
            Fogo -> fromJust $ lookup ("animacaoTorreFogo" ++ show iteracoes) ts
    in translate x (y+80) textura

desenhaAnimacaoInimigo :: Inimigo -> [Textura] -> Picture
desenhaAnimacaoInimigo i ts =
    let its = iteracoesDesdeInicioAnimacaoInimigo i
        textura = fromJust $ lookup ("guerreiro" ++ show (ceiling $ int2Float(its) / 4)) ts
    in  Pictures [textura, text $ show its]

desenhaPortais :: [Portal] -> Picture -> [Picture]
desenhaPortais [] _ = []
desenhaPortais (p:ps) textura = 
    let (x,y) = conversaoCoordsGloss $ posicaoPortal p
    in translate x (y  + (128-64)/2) textura : desenhaPortais ps textura
    
desenhaLoja :: Loja -> [Textura] -> Picture
desenhaLoja loja ts = Pictures [iconeLoja, store, fundoTorre1, fundoTorre2, fundoTorre3, Pictures $ map desenhaTorre loja]
    where x = -805
          y = 100
          espacamento = 200
          tamanhoTorre = 0.70
          tamanhoCreditos = 1
          --moldura = Translate (-730) (-60) $ scale 10 25 $ fromJust $ lookup "moldura" ts
          --iconeLoja = Translate (-750) 300 $ scale 5 2 $ fromJust $ lookup "iconeLoja" ts
          lojaFundo = Translate (-750) (-60) $ scale 2 2 $ fromJust $ lookup "lojaFundo" ts
          iconeLoja = Translate (-845) (20) $ scale 5.5 5.5  $ fromJust $ lookup "iconeLoja" ts
          fundoTorre1 =  Translate (-730) (100) $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts
          fundoTorre2 = Translate (-730) (-100) $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts
          fundoTorre3 = Translate (-730) (-300) $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts
          store = Translate (-750) 270 $ scale 1.5 1.5 $ fromJust $ lookup "Store" ts
          desenhaTorre :: (Creditos, Torre) -> Picture
          desenhaTorre (cs,t) = case tipoProjetil $ projetilTorre t of
            Gelo -> Pictures [translate x y $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreGelo" ts), translate (-740) 90 $ scale tamanhoCreditos tamanhoCreditos $ string2FonteNumeros (show $ cs) ts]
            Resina -> Pictures [translate x (y-espacamento) $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreResina" ts), translate (-740) (90-espacamento) $ scale tamanhoCreditos tamanhoCreditos $ string2FonteNumeros (show $ cs) ts]
            Fogo -> Pictures [translate x (y-2*espacamento) $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreFogo" ts), translate (-740) (90-2*espacamento) $ scale tamanhoCreditos tamanhoCreditos $ string2FonteNumeros (show $ cs) ts]

desenhaPerfilJogador :: Jogo -> Base -> [Textura] -> Picture 
desenhaPerfilJogador j b ts = Pictures [creditosJogador, creditos, iconeVida, vidaBaseJg, perfil, iconePausa, iconeHome, iconeJogador, nInimigos]
   where creditosJogador = Translate 750 210 $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts 
         creditos = Translate 750 202 $ scale 1 1 $ string2FonteNumeros (show $ creditosBase b) ts
         iconeVida = Translate 750 100 $ scale 3.5 3.5 $ fromJust $ lookup "iconeVidaJg" ts
         vidaBaseJg = Translate 740 120 $ scale 1 1 $ string2FonteNumeros (show $ ceiling $ vidaBase b) ts 
         perfil = Translate 680 210 $ scale 1 1 $ fromJust $ lookup "perfil" ts 
         iconePausa = Pictures [Translate 650 460 $ scale 2 2 $ fromJust $ lookup "botaoPausa" ts, Translate 740 460 $ scale 3 3 $ fromJust $ lookup "iconePausa" ts, Translate 740 460 $ scale 0.6 0.6 $ fromJust $ lookup "frasePausa" ts]
         iconeHome = Pictures [Translate 650 400 $ scale 2.5 2.5 $ fromJust $ lookup "iconeHome" ts]
         iconeJogador = Pictures [Translate 660 55 $ scale 5.5 5.5  $ fromJust $ lookup "iconeLoja" ts, Translate 760 300 $ scale 1.5 1.5 $ fromJust $ lookup "player" ts]
         nInimigos = Pictures [Translate 670 40 $ scale 3.5 3.5 $ fromJust $ lookup "nInimigos" ts , Translate 780 40 $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts, scale 1 1 $ Translate 750 55 $ string2FonteNumeros (show $ length (inimigosJogo j)) ts ]
-- translate (-960+16*10) (540-16*10) $ scale 10 10 (ts!!10) -- painel
-- translate (x) (y+30) $ scale 0.1 0.1 $ text $ show $ vidaInimigo inimigo

desenhaPausa :: [Textura] -> Picture 
desenhaPausa ts = Pictures [fundo, iconePausa, fraseContinuar, iconeBackMenu]
    where fundo = translate 0 0 $ color (withAlpha 0.8 black) $ rectangleSolid 1920 1080 
          iconePausa = Translate 0 0 $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          fraseContinuar = Translate 0 1 $ scale 0.8 0.8 $ fromJust $ lookup "fraseContinuar" ts
          iconeBackMenu = Translate 0 (-90) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
