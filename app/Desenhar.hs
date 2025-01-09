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
     Jogando -> Pictures [desenhaJogo it, Translate 0 0 $ scale 1 1 $ text $ show $ modoJogo it] 
     EscolhendoTorre -> Pictures [desenhaEscolhendoTorre it] 
     Comprando -> desenhaComprando it
     Pausado -> Pictures [desenhaJogo it, desenhaPausa ts]  
     CriandoMapa -> desenhaCriandoMapa it
     EscolhendoOndas -> desenhaEscolhendoOnda it 
     EscolhendoIG -> desenhaEscolhendoOnda it 
     EscolhendoIM -> desenhaEscolhendoOnda it
     NivelPassado -> Pictures [desenhaJogo it, desenhaNivelPassado it]
     GameOver -> Pictures [desenhaJogo it, desenhaGameOver it ]
     YouWon -> Pictures [desenhaJogo it, desenhaYouWon it]
     MensagemErro -> Pictures [desenhaCriandoMapa it, desenhaMensagemErro it]
  where ts = texturasIT it

string2FonteNumeros :: String -> [Textura] -> Picture
string2FonteNumeros s ts = Pictures $ auxString2FonteNumeros s ts 0

auxString2FonteNumeros :: String -> [Textura] -> Float -> [Picture]
auxString2FonteNumeros [] _ _ = []
auxString2FonteNumeros (h:t) ts ac = 
    let resultado = lookup ("numero" ++ [h]) ts
    in
        case resultado of
            Nothing -> [translate (ac*espacamento) 0 $ fromJust $ lookup "numero0" ts]
            _ -> (translate (ac*espacamento) 0 $ fromJust resultado) : auxString2FonteNumeros t ts (ac+1)
    where espacamento = 13

desenhaMensagemErro :: ImmutableTowers -> Picture
desenhaMensagemErro it = Pictures [fromJust $ lookup "mensagemErro" (texturasIT it), color (withAlpha 0.5 black) $ rectangleSolid 1920 1080]

desenhaNivelPassado :: ImmutableTowers -> Picture
desenhaNivelPassado it = Pictures [fundo, iconeBackToMenu, iconeNextLevel, iconeRestart, fraseLevelWon, fraseBackToMenu, fraseNextLevel, fraseRestart, seta, bb ]
    where ts = texturasIT it
          fundo = translate 0 0 $ color (withAlpha 0.7 orange) $ rectangleSolid 1920 1080 
          fraseLevelWon = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseLevelWon" ts
          fraseBackToMenu =  Translate (-350) (-250) $ scale 0.7 0.7 $ fromJust $ lookup "fraseBackToMenu" ts
          fraseNextLevel = Translate (0) (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseNextLevel" ts 
          fraseRestart = Translate (350) (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseRestart" ts
          (x,y) = botaoNivelPassado it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeNextLevel = Translate (0) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate (350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts
          bb = translate 0 0 $ scale 1 1 $ text $ show $ botaoNivelPassado it 

desenhaGameOver :: ImmutableTowers -> Picture
desenhaGameOver it = Pictures [fundo, iconeBackToMenu, iconeRestart, fraseGameOver, fraseBackToMenu, fraseRestart, seta]
    where ts = texturasIT it
          fundo = translate 0 0 $ color (withAlpha 0.8 red) $ rectangleSolid 1920 1080 
          fraseGameOver = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseGameOver" ts
          fraseBackToMenu =  Translate (-350) (-250) $ fromJust $ lookup "fraseBackToMenu" ts
          fraseRestart = Translate (350) (-250) $ fromJust $ lookup "fraseRestart" ts 
          (x,y) = botaoGameOver it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate (350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 

desenhaYouWon :: ImmutableTowers -> Picture
desenhaYouWon it = Pictures [fundo, 
                             iconeBackToMenu, 
                             iconeRestart, 
                             fraseYouWon, 
                             fraseBackToMenu, 
                             fraseRestart, 
                             seta
                            ]
    where ts = texturasIT it 
          fundo = translate 0 0 $ color (withAlpha 0.8 orange) $ rectangleSolid 1920 1080 
          fraseYouWon = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseYouWon" ts
          fraseBackToMenu =  Translate (-350) (-250) $ fromJust $ lookup "fraseBackToMenu" ts
          fraseRestart = Translate (350) (-250) $ fromJust $ lookup "fraseRestart" ts
          (x,y) = botaoGameOver it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts
          iconeRestart = Translate (350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 


desenhaEscolhendoOnda :: ImmutableTowers -> Picture 
desenhaEscolhendoOnda it = Pictures [fundo, 
                                     botao1, 
                                     botao2, 
                                     botao3, 
                                     setaCima1, 
                                     setaCima2, 
                                     setaCima3, 
                                     setaBaixo1, 
                                     setaBaixo2, 
                                     setaBaixo3, 
                                     enO, 
                                     enIG, 
                                     enIM
                                    ]
    where 
        ts = texturasIT it 
        (nO, nIG, nIM) = escolhendoParametros it
        enO = translate (-135) 25 $ scale 1.5 1.5 $ string2FonteNumeros (show nO) ts
        enIG = translate (-35) 25 $ scale 1.5 1.5 $ string2FonteNumeros (show nIG) ts
        enIM = translate 58 25 $ scale 1.5 1.5 $ string2FonteNumeros (show nIM) ts
        botao1 = translate (-100) 0 $ scale 5 5 $ fromJust $ lookup "botaoQ" ts
        botao2 = translate 0 0 $ scale 5 5 $ fromJust $ lookup "botaoQ" ts
        botao3 = translate 100 0 $ scale 5 5 $ fromJust $ lookup "botaoQ" ts
        setaCima1 = translate 100 90 $ scale 4 4 $ fromJust $ lookup "setaCima" ts
        setaCima2 = translate (-5) 90 $ scale 4 4 $ fromJust $ lookup "setaCima" ts
        setaCima3 = translate (-104) 90 $ scale 4 4 $ fromJust $ lookup "setaCima" ts
        setaBaixo1 = translate 100 (-90) $ scale 4 4 $ fromJust $ lookup "setaBaixo" ts
        setaBaixo2 = translate (-5) (-90) $ scale 4 4 $ fromJust $ lookup "setaBaixo" ts
        setaBaixo3 = translate (-105) (-90) $ scale 4 4 $ fromJust $ lookup "setaBaixo" ts
        fundo = Pictures [desenhaCriandoMapa it, translate 0 0 $ color (withAlpha 0.8 black) $ rectangleSolid 1920 1080]

desenhaCriandoMapa :: ImmutableTowers -> Picture 
desenhaCriandoMapa it = 
    Pictures [fundo,
              desenhaLoja loja ts, 
              desenhaPerfilJogador jogo base ts, 
              desenhaListaTerreno lt ts, 
              Pictures picPortais, 
              picBase, 
              desenhaSelecao (x,y)
             ] 
  where 
    (x,y) = posicaoSelecionadaMapa it
    jogo = jogoIT it 
    loja = lojaJogo jogo
    ts = texturasIT it 
    base = baseJogo jogo
    lt = listaTerreno it
    pps = listaPortais it  
    picBase = if baseCriada it
              then desenhaBase (baseJogo jogo) (fromJust $ lookup "base" ts)
              else Blank
    picPortais = desenhaPortais pps (fromJust $ lookup "portal" ts)
    fundo = Translate 0 0 $ color (withAlpha 0.5 black) $ rectangleSolid 1024 1024 

desenhaListaTerreno :: [(Posicao, Terreno)] -> [Textura] -> Picture
desenhaListaTerreno lt ts = Pictures $ map (`desenhaUMterreno` ts) lt 

desenhaUMterreno :: (Posicao, Terreno) -> [Textura] -> Picture 
desenhaUMterreno ((x,y), terreno) ts = 
    let (x1,y1) = conversaoCoordsGloss (x,y)
    in Translate x1 y1 $ desenhaTerreno terreno ts

desenhaTerreno :: Terreno -> [Textura] -> Picture
desenhaTerreno Terra ts = fromJust $ lookup "terra" ts
desenhaTerreno Relva ts = fromJust $ lookup "relva" ts
desenhaTerreno Agua ts = fromJust $ lookup "agua" ts 

desenhaComprando :: ImmutableTowers -> Picture 
desenhaComprando it = Pictures [desenhaJogo it, 
                                desenhaSelecao selec]
  where
    selec = posicaoSelecionadaMapa it

desenhaEscolhendoMenu :: ImmutableTowers -> Picture 
desenhaEscolhendoMenu it = Pictures [desenhaSelecaoLoja selec ts]
  where selec = botaoMenu it 
        ts = texturasIT it

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
    [fundoTela,
    translate 0 250 $ scale 1 1 $ itTitulo, 
    translate 0 0 $ scale 5 5 $ botaoMenu, 
    translate 0 0 $ scale 1.2 1.2 $ fm, 
    translate (0) (-100) $ scale 5 5 $ botaoMenu,
    translate 0 (-100) $ scale 1.1 1.1 $ im, 
    translate  (0) (-200) $ scale 5 5 $ botaoMenu,
    translate 0 (-200) $ scale 1.2 1.2 $ cm, 
    translate 0 (-300) $ scale 5 5 $ botaoMenu,
    translate 0 (-300) $ scale 1.2 1.2 $ tt,
    desenhaEscolhendoMenu it  
         ]
     where texturas = texturasIT it 
           fundoTela = fromJust $ lookup "fundoJogo" texturas 
           botaoMenu = fromJust $ lookup "iconeLoja" texturas
           itTitulo = fromJust $ lookup "it" texturas 
           fm = fromJust $ lookup "fm" texturas 
           im = fromJust $ lookup "im" texturas 
           cm = fromJust $ lookup "cm" texturas 
           tt = fromJust $ lookup "tt" texturas
    


desenhaJogo :: ImmutableTowers -> Picture
desenhaJogo it = Pictures [picMapa, 
                           picMolduraMapa, 
                           picInimigo,
                           Pictures picPortais, 
                           picLoja, 
                           picBase, 
                           picTorre, 
                           creditosJog, 
                           moldBaixo,
                           nivel
                          ]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          picMolduraMapa = desenhaMolduraMapa texturas
          moldBaixo = translate 0 (-520) $ scale 1 1 $ (fromJust $ lookup "moldBaixo" texturas)
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
          nivel = translate 0 0 $ scale 0.5 0.5 $ text $ show $ nivelJogoInfinito it



desenhaMolduraMapa :: [Textura] -> Picture
desenhaMolduraMapa ts = Pictures [moldCima]
    where moldCima = translate 0 0 $ scale 1 1 $ (fromJust $ lookup "molduraMapa2" ts)


desenhaMapa :: Mapa -> [Textura] -> Picture
desenhaMapa mapa textures =
    let t = getMapaTexturas mapa textures
    in Pictures [translate 0 0 (fromJust $ lookup "fundoJogo" textures), 
                 pictures [translate ((fromInteger x * fromInteger lado )-7.5*64) ((fromInteger y * fromInteger lado) +7.5*64 ) ((t!!abs (fromInteger y))!!fromInteger x) | (x,y) <- positions]]

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

desenhaInimigos :: [Inimigo] -> [Textura] -> Picture
desenhaInimigos inimigos texturas = Pictures $ map (`desenhaUmInimigo` texturas) inimigos 

desenhaUmInimigo :: Inimigo -> [Textura] -> Picture 
desenhaUmInimigo inimigo texturas = 
    let (x, y) = conversaoCoordsGloss $ posicaoInimigo inimigo
        comprimentoNumeroVidaPxs = int2Float (length (show $ ceiling $ vidaInimigo inimigo) * 13)
        offsetNumeroVida = (comprimentoNumeroVidaPxs+27+18)/2*0.5 --metade do comprimento da vida, da largura do inimigo e da largura do coração, escalado a 0.5
        numeroDaVida = translate (x-offsetNumeroVida) (y+40) $ scale 0.5 0.5 $ string2FonteNumeros (show $ ceiling $ vidaInimigo inimigo) texturas
        ataqueInimig1 = Translate x y $ scale 1 1 ( text ( show ( ataqueInimigo inimigo)))
        coracaoVida = translate (x+offsetNumeroVida) (y+40-(16/2*0.7)) $ scale 0.7 0.7 $ fromJust $ lookup "vida" texturas
        textura = desenhaAnimacaoInimigo inimigo texturas
    in Pictures [translate x y textura, 
                 numeroDaVida, 
                 coracaoVida, 
                 ataqueInimig1
                ]

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
        textura = case tipoInimigo i of
            Guerreiro -> fromJust $ lookup ("guerreiro" ++ show (ceiling $ int2Float(its) / 4)) ts
            MulherLanca -> fromJust $ lookup ("mulherLanca" ++ show (ceiling $ int2Float(its) / 4)) ts
    in  Pictures [textura]

desenhaPortais :: [Portal] -> Picture -> [Picture]
desenhaPortais [] _ = []
desenhaPortais (p:ps) textura = 
    let (x,y) = conversaoCoordsGloss $ posicaoPortal p
    in translate x (y  + (128-64)/2) textura : desenhaPortais ps textura
    
desenhaLoja :: Loja -> [Textura] -> Picture
desenhaLoja loja ts = Pictures [iconeLoja, 
                                store, 
                                fundoTorre1, 
                                fundoTorre2, 
                                fundoTorre3, 
                                Pictures $ map desenhaTorre loja]
    where x = -805
          y = 100
          espacamento = 200
          tamanhoTorre = 0.70
          tamanhoCreditos = 1
          iconeLoja = Translate (-750) 270 $ scale 5.5 5.5  $ fromJust $ lookup "iconeLoja" ts
          fundoTorre1 =  Translate (-730) 100 $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts
          fundoTorre2 = Translate (-730) (-100) $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts
          fundoTorre3 = Translate (-730) (-300) $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts
          store = Translate (-750) 270 $ scale 1.5 1.5 $ fromJust $ lookup "Store" ts
          desenhaTorre :: (Creditos, Torre) -> Picture
          desenhaTorre (cs,t) = case tipoProjetil $ projetilTorre t of
            Gelo -> Pictures [translate x y $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreGelo" ts), 
                              translate (-740) 90 $ scale tamanhoCreditos tamanhoCreditos $ string2FonteNumeros (show cs) ts
                             ]
            Resina -> Pictures [translate x (y-espacamento) $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreResina" ts), 
                                translate (-740) (90-espacamento) $ scale tamanhoCreditos tamanhoCreditos $ string2FonteNumeros (show cs) ts
                               ]
            Fogo -> Pictures [translate x (y-2*espacamento) $ scale tamanhoTorre tamanhoTorre (fromJust $ lookup "torreFogo" ts), 
                              translate (-740) (90-2*espacamento) $ scale tamanhoCreditos tamanhoCreditos $ string2FonteNumeros (show cs) ts
                             ]

desenhaPerfilJogador :: Jogo -> Base -> [Textura] -> Picture 
desenhaPerfilJogador j b ts = Pictures [creditosJogador, 
                                        creditos, 
                                        iconeVida, 
                                        vidaBaseJg, 
                                        perfil, 
                                        iconePausa, 
                                        iconeHome, 
                                        iconeJogador, 
                                        nInimigos
                                        ]
   where creditosJogador = Translate 750 210 $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts 
         creditos = Translate 750 202 $ scale 1 1 $ string2FonteNumeros (show $ creditosBase b) ts
         iconeVida = Translate 750 100 $ scale 3.5 3.5 $ fromJust $ lookup "iconeVidaJg" ts
         vidaBaseJg = Translate 740 120 $ scale 1 1 $ string2FonteNumeros (show $ ceiling $ vidaBase b) ts 
         perfil = Translate 680 210 $ scale 1 1 $ fromJust $ lookup "perfil" ts 
         iconePausa = Pictures [Translate 650 460 $ scale 2 2 $ fromJust $ lookup "botaoPausa" ts, 
                                Translate 740 460 $ scale 3 3 $ fromJust $ lookup "iconePausa" ts, 
                                Translate 740 460 $ scale 0.6 0.6 $ fromJust $ lookup "frasePausa" ts
                               ]
         iconeHome = Pictures [Translate 650 400 $ scale 2.5 2.5 $ fromJust $ lookup "iconeHome" ts]
         iconeJogador = Pictures [Translate 760 300 $ scale 5.5 5.5  $ fromJust $ lookup "iconeLoja" ts, 
                                  Translate 760 300 $ scale 1.5 1.5 $ fromJust $ lookup "player" ts
                                 ]
         nInimigos = Pictures [Translate 670 40 $ scale 3.5 3.5 $ fromJust $ lookup "nInimigos" ts , 
                               Translate 780 40 $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts, 
                               Translate 750 55 $ scale 1 1 $ string2FonteNumeros (show $ length (inimigosJogo j)) ts 
                              ]
-- translate (-960+16*10) (540-16*10) $ scale 10 10 (ts!!10) -- painel
-- translate (x) (y+30) $ scale 0.1 0.1 $ text $ show $ vidaInimigo inimigo

desenhaPausa :: [Textura] -> Picture 
desenhaPausa ts = Pictures [fundo, 
                            iconePausa, 
                            frasePaused, 
                            iconeBackMenu
                            ]
    where fundo = translate 0 0 $ color (withAlpha 0.8 black) $ rectangleSolid 1920 1080 
          iconePausa = Translate 0 0 $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          frasePaused = Translate 0 1 $ scale 0.8 0.8 $ fromJust $ lookup "frasePaused" ts
          iconeBackMenu = Translate 0 (-90) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 