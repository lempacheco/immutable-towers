module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Data.Maybe (fromJust)
import GHC.Float (int2Float)
import Tarefa2 

lado :: Integer
lado = 64

altura :: Integer
altura = 64*16

comprimento :: Integer
comprimento = 64*16

desenha :: ImmutableTowers -> Picture
desenha it = Pictures [desenhaPrimeiroJogador it, desenhaSegundoJogador it]

desenhaPrimeiroJogador :: ImmutableTowers -> Picture
desenhaPrimeiroJogador it = case estadoIT it of
     Menu -> desenhaMenu  it 
     EscolhendoTorre -> Pictures [desenhaEscolhendoTorre it]
     Comprando -> desenhaComprando it
     Pausado -> Pictures [desenhaJogo it, desenhaPausa it]  
     CriandoMapa -> desenhaCriandoMapa it
     EscolhendoOndas -> desenhaEscolhendoOnda it 
     EscolhendoIG -> desenhaEscolhendoOnda it 
     EscolhendoIM -> desenhaEscolhendoOnda it
     NivelPassado -> Pictures [desenhaJogo it, desenhaNivelPassado it]
     GameOver -> Pictures [desenhaJogo it, desenhaGameOver it ]
     YouWon -> Pictures [desenhaJogo it, desenhaYouWon it]
     MensagemErro -> if modoJogo it == MapaCriado then Pictures [desenhaCriandoMapa it, desenhaMensagemErro it] else Pictures [desenhaMenu it, desenhaMensagemErro it]
     YouWon1 -> Pictures [desenhaJogo it, desenhaYouWon1 it]
     Tutorial -> desenhaTutorial it ts
     TutorialEscolhendoTorre -> desenhaTutorial it ts 
     TutorialComprando -> desenhaTutorial it ts 
     Jogando -> Pictures [desenhaJogo it] 
     Costumizar -> desenhaCostumizar it ts
     
  where ts = texturasIT it

desenhaSegundoJogador :: ImmutableTowers -> Picture
desenhaSegundoJogador it = if (estadoIT it == Jogando || estadoIT it == EscolhendoTorre || estadoIT it == Comprando) then case estadoIT2 it of 
    EscolhendoTorre2 -> Pictures [desenhaEscolhendoTorre2 it]
    Comprando2 -> Pictures [desenhaComprando2 it]
    _  -> Pictures []
 else Pictures []
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

desenhaCostumizar :: ImmutableTowers -> [Textura] -> Picture
desenhaCostumizar it ts = Pictures [
        fromJust $ lookup "menuCustomize" ts,
        Translate (-300) 350 $ Scale 3 3 (fromJust $ lookup "guerreiro0" ts),
        Translate 300 350 $ Scale 3 3 (fromJust $ lookup "viking0" ts),
        Translate (-300) (-50) $ Scale 3 3 (fromJust $ lookup "mulherLanca0" ts),
        Translate 300 (-50) $ Scale 3 3 (fromJust $ lookup "guerreiraMulher0" ts),
        Translate (-500) (-350) $ Scale 2 2 (fromJust $ lookup "perfilGuerreiro" ts),
        Translate 0 (-350) $ Scale 2 2 (fromJust $ lookup "perfilViking" ts),
        Translate 500 (-350) $ Scale 2 2 (fromJust $ lookup "perfilMulherLanca" ts),
        Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts,
        Translate (650) (-450) $ fromJust $ lookup "costumizarVoltar" ts
    ]
    where (x,y) = selecaoCostumizar it

desenhaNivelPassado :: ImmutableTowers -> Picture
desenhaNivelPassado it = Pictures [fundo, iconeBackToMenu, iconeNextLevel, iconeRestart, fraseLevelWon, fraseBackToMenu, fraseNextLevel, fraseRestart, seta]
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
           

desenhaGameOver :: ImmutableTowers -> Picture
desenhaGameOver it = Pictures [fundo, 
                               iconeBackToMenu, 
                               iconeRestart, 
                               fraseGameOver, 
                               fraseBackToMenu, 
                               fraseRestart, 
                               seta
                              ]
    where ts = texturasIT it
          fundo = translate 0 0 $ color (withAlpha 0.8 red) $ rectangleSolid 1920 1080 
          fraseGameOver = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseGameOver" ts
          fraseBackToMenu =  Translate (-350) (-250) $ fromJust $ lookup "fraseBackToMenu" ts
          fraseRestart = Translate (350) (-250) $ fromJust $ lookup "fraseRestart" ts 
          (x,y) = botaoGameOver it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate (350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 

desenhaYouWon1 :: ImmutableTowers -> Picture
desenhaYouWon1 it = Pictures [fundo, 
                               iconeBackToMenu, 
                               iconeRestart, 
                               fraseGameOver, 
                               fraseBackToMenu, 
                               fraseRestart, 
                               seta
                              ]
    where ts = texturasIT it
          fundo = translate 0 0 $ color (withAlpha 0.8 orange) $ rectangleSolid 1920 1080 
          fraseGameOver = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseYouWon" ts
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
                             iconeNextLevel, 
                             fraseYouWon, 
                             fraseBackToMenu, 
                             fraseRestart,
                             fraseNextLevel,
                             seta
                            ]
    where ts = texturasIT it 
          fundo = translate 0 0 $ color (withAlpha 0.8 orange) $ rectangleSolid 1920 1080 
          fraseYouWon = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseYouWon" ts
          fraseBackToMenu =  Translate (-350) (-250) $ scale 0.7 0.7 $ fromJust $ lookup "fraseBackToMenu" ts
          fraseNextLevel = Translate (0) (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseNextLevel" ts 
          fraseRestart = Translate (350) (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseRestart" ts
          (x,y) = botaoNivelPassado it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeNextLevel = Translate (0) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate (350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 


desenhaEscolhendoOnda :: ImmutableTowers -> Picture 
desenhaEscolhendoOnda it = case estadoIT it of 
    EscolhendoOndas -> Pictures [fundo, 
                                 botao1, 
                                 botao2, 
                                 botao3, 
                                 setaCima1, 
                                 setaCima2, 
                                 setaCima3, 
                                 setaBaixo1, 
                                 setaBaixo2, 
                                 setaBaixo3, 
                                 nOndas,
                                 setaLado,
                                 enO, 
                                 enIG, 
                                 enIM]
    EscolhendoIG -> Pictures  [fundo, 
                                 botao1, 
                                 botao2, 
                                 botao3, 
                                 setaCima1, 
                                 setaCima2, 
                                 setaCima3, 
                                 setaBaixo1, 
                                 setaBaixo2, 
                                 setaBaixo3, 
                                 ninimigoM,
                                 Translate (-10) 0 $ setaLadoL,
                                 Translate 250 0 $ setaLado,
                                 enO, 
                                 enIG, 
                                 enIM]
    EscolhendoIM -> Pictures  [fundo, 
                                 botao1, 
                                 botao2, 
                                 botao3, 
                                 setaCima1, 
                                 setaCima2, 
                                 setaCima3, 
                                 setaBaixo1, 
                                 setaBaixo2, 
                                 setaBaixo3, 
                                 ninimigoF,
                                 Translate 240 0 $ setaLadoL,
                                 enO, 
                                 enIG, 
                                 enIM]
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
        nOndas = translate (-250) (-180) $ scale 0.7 0.7 $ fromJust $ lookup "nOndas" ts
        ninimigoM = translate (0) (-180) $ scale 0.5 0.5 $ fromJust $ lookup "ninimigoM" ts 
        ninimigoF = translate (250) (-180) $ scale 0.5 0.5 $ fromJust $ lookup "ninimigoF" ts
        setaLado = translate (-120) (-180) $ scale 3 3 $ fromJust $ lookup "seta" ts
        setaLadoL = translate (-120) (-180) $ scale 3 3 $ rotate 180 $ fromJust $ lookup "seta" ts
        fundo = Pictures [desenhaCriandoMapa it, translate 0 0 $ color (withAlpha 0.8 black) $ rectangleSolid 1920 1080]

desenhaCriandoMapa :: ImmutableTowers -> Picture 
desenhaCriandoMapa it = 
    Pictures [fundo,
              desenhaLoja loja ts, 
              desenhaPerfilJogador it ts, 
              desenhaListaTerreno lt ts, 
              Pictures picPortais, 
              picBase, 
              desenhaSelecao (x,y),
              modo
             ] 
  where 
    (x,y) = posicaoSelecionadaMapa it
    jogo = jogoIT it 
    loja = lojaJogo jogo
    ts = texturasIT it 
    lt = listaTerreno it
    pps = listaPortais it  
    picBase = if baseCriada it
              then desenhaBase (baseJogo jogo) (fromJust $ lookup "base" ts)
              else Blank
    picPortais = desenhaPortais pps (fromJust $ lookup "portal" ts)
    fundo = Translate 0 0 $ color (withAlpha 0.5 black) $ rectangleSolid 1024 1024 
    modo = Translate 780 (-20) $ scale 0.8 0.8 $ fromJust $ lookup "criarMapa" ts

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

desenhaComprando2 :: ImmutableTowers -> Picture 
desenhaComprando2 it = Pictures [desenhaSelecaoSndJog selec2]
  where
    selec2 = posicaoSelecionadaMapaSndJog it 

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

desenhaEscolhendoTorre2 :: ImmutableTowers -> Picture 
desenhaEscolhendoTorre2 it = Pictures [desenhaSelecaoLoja selec2 ts]
  where 
        selec2 = produtoLoja2 it 
        ts = texturasIT it

desenhaSelecaoLoja :: (Float, Float) -> [Textura] -> Picture
desenhaSelecaoLoja (x,y) ts = Pictures [Translate x y $ scale 3 3 $ fromJust $ lookup "seta" ts]

-- Função para desenhar a seleção no mapa
desenhaSelecao :: (Float, Float) -> Picture
desenhaSelecao pos =
    let (x,y) = conversaoCoordsGloss pos
    in translate x y $
       color (withAlpha 0.5 red) $ rectangleSolid 64 64

desenhaSelecaoSndJog :: (Float, Float) -> Picture
desenhaSelecaoSndJog pos =
    let (x,y) = conversaoCoordsGloss pos
    in translate x y $
       color (withAlpha 0.5 blue) $ rectangleSolid 64 64

desenhaMenu :: ImmutableTowers -> Picture 
desenhaMenu it = Pictures 
    [fundoTela,
    translate 0 250 $ scale 1 1 $ itTitulo, 
    translate 0 0 $ scale 5 5 $ botaoMenu, 
    translate 0 0 $ fm, 
    translate (0) (-100) $ scale 5 5 $ botaoMenu,
    translate 0 (-100) $ im, 
    translate  (0) (-200) $ scale 5 5 $ botaoMenu,
    translate 0 (-200) $ cm, 
    translate 0 (-300) $ scale 5 5 $ botaoMenu,
    translate 0 (-300) $ scale 1.2 1.2 $ tt,
    translate 0 (-400) $ scale 5 5 $ botaoMenu,
    translate 0 (-400) $ scale 1.2 1.2 $ costumize,
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
           costumize = fromJust $ lookup "customize" texturas
    


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
                           picModoJogo
                          ]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          picMolduraMapa = desenhaMolduraMapa texturas
          moldBaixo = translate 0 (-520) $ scale 1 1 $ (fromJust $ lookup "moldBaixo" texturas)
          texturas = texturasIT it
          picBase = desenhaBase base (fromJust $ lookup "base" texturas)
          base = baseJogo jogo
          picInimigo = Pictures $ desenhaInimigos inimigos it texturas
          inimigos = inimigosJogo jogo
          picPortais = desenhaPortais portais (fromJust $ lookup "portal" texturas)
          portais = portaisJogo jogo
          picTorre = desenhaTorres torres texturas 
          torres = torresJogo jogo
          picLoja = desenhaLoja loja texturas
          loja = lojaJogo jogo
          creditosJog = desenhaPerfilJogador it texturas 
          picModoJogo = desenhaModoJogo it texturas


desenhaMolduraMapa :: [Textura] -> Picture
desenhaMolduraMapa ts = Pictures [moldCima]
    where moldCima = translate 0 0 $ scale 1 1 $ (fromJust $ lookup "molduraMapa" ts)


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

desenhaInimigos :: [Inimigo] -> ImmutableTowers -> [Textura] -> [Picture]
desenhaInimigos [] _ _ = []
desenhaInimigos (i:is) it texturas = desenhaUmInimigo i it texturas : desenhaInimigos is it texturas

desenhaUmInimigo :: Inimigo -> ImmutableTowers -> [Textura] -> Picture 
desenhaUmInimigo inimigo it texturas = 
    let (x, y) = conversaoCoordsGloss $ posicaoInimigo inimigo
        comprimentoNumeroVidaPxs = int2Float (length (show $ ceiling $ vidaInimigo inimigo) * 13)
        offsetNumeroVida = (comprimentoNumeroVidaPxs+27+18)/2*0.5 --metade do comprimento da vida, da largura do inimigo e da largura do coração, escalado a 0.5
        numeroDaVida = translate (x-offsetNumeroVida) (y+40) $ scale 0.5 0.5 $ string2FonteNumeros (show $ ceiling $ vidaInimigo inimigo) texturas
        coracaoVida = translate (x+offsetNumeroVida) (y+40-(16/2*0.7)) $ scale 0.7 0.7 $ fromJust $ lookup "vida" texturas
        textura = desenhaAnimacaoInimigo inimigo it texturas
        indicativoProjeteis = Translate x (y + 50) $ desenhaIndicativoProjeteis inimigo texturas
    in Pictures [translate x y textura, 
                 numeroDaVida, 
                 coracaoVida,
                 indicativoProjeteis,
                 Translate x (y+100) $ Scale 0.2 0.2 $ Text (show (projeteisInimigo inimigo))
                ]

desenhaIndicativoProjeteis :: Inimigo -> [Textura] -> Picture
desenhaIndicativoProjeteis i ts
    | elem Gelo tiposProjs && elem Resina tiposProjs && elem Fogo tiposProjs = Pictures [Translate (-30) 0 iGelo, Translate 0 0 iResina, Translate 30 0 iFogo]
    | elem Gelo tiposProjs && elem Resina tiposProjs = Pictures [Translate (-10) 0 iGelo, Translate 10 0 iResina]
    | elem Gelo tiposProjs && elem Fogo tiposProjs = Pictures [Translate (-10) 0 iGelo, Translate 10 0 iFogo]
    | elem Fogo tiposProjs && elem Resina tiposProjs = Pictures [Translate (-10) 0 iFogo, Translate 10 0 iResina]
    | elem Gelo tiposProjs = Pictures [iGelo]
    | elem Fogo tiposProjs = Pictures [iFogo]
    | elem Resina tiposProjs = Pictures [iResina]
    | otherwise = Pictures []
    where tiposProjs = getTiposProjsInimigo i
          iGelo = fromJust $ lookup "indicativoProjetilGelo" ts
          iFogo = fromJust $ lookup "indicativoProjetilFogo" ts
          iResina = fromJust $ lookup "indicativoProjetilResina" ts

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

desenhaAnimacaoInimigo :: Inimigo -> ImmutableTowers -> [Textura] -> Picture
desenhaAnimacaoInimigo i it ts =
    let its = iteracoesDesdeInicioAnimacaoInimigo i
        textura = case tipoInimigo i of
            Homem -> fromJust $ lookup (inimigoHomem it ++ show (ceiling $ int2Float(its) / 4)) ts
            Mulher -> fromJust $ lookup (inimigoMulher it ++ show (ceiling $ int2Float(its) / 4)) ts
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

desenhaPerfilJogador :: ImmutableTowers -> [Textura] -> Picture 
desenhaPerfilJogador it ts  
    | (modoJogo it == MapaCriado) || (estadoIT it == Tutorial) || (estadoIT it == TutorialEscolhendoTorre) || (estadoIT it == TutorialComprando)
               =  Pictures [creditosJogador, 
                            creditos, 
                            iconeVida, 
                            vidaBaseJg, 
                            perfilJogador, 
                            iconePausa, 
                            iconeHome, 
                            iconeJogador, 
                            nInimigos, 
                            nivelJogador, 
                            modoJogo1
                            ]
    | otherwise         = Pictures [creditosJogador, 
                            creditos, 
                            iconeVida, 
                            vidaBaseJg, 
                            perfilJogador, 
                            iconePausa, 
                            iconeHome, 
                            iconeJogador, 
                            nInimigos, 
                            nivelJogador, 
                            modoJogo1,
                            multiplayer1
                            ]
   where j = jogoIT it 
         b = baseJogo j 
         creditosJogador = Translate 750 210 $ scale 4 4 $ fromJust $ lookup "creditosJogador" ts 
         creditos = Translate 750 202 $ scale 1 1 $ string2FonteNumeros (show $ creditosBase b) ts
         iconeVida = Translate 750 100 $ scale 3.5 3.5 $ fromJust $ lookup "iconeVidaJg" ts
         vidaBaseJg = Translate 740 120 $ scale 1 1 $ string2FonteNumeros (show $ ceiling $ vidaBase b) ts 
         perfilJogador = Translate 680 210 $ scale 1 1 $ fromJust $ lookup (perfil it) ts 

         iconePausa = Pictures [Translate 650 460 $ scale 2 2 $ fromJust $ lookup "botaoPausa" ts, 
                                Translate 770 460 $ scale 4 3.5 $ fromJust $ lookup "iconePausa" ts, 
                                Translate 770 460 $ scale 0.5 0.5 $ fromJust $ lookup "frasePausa" ts
                               ]
         iconeHome = Pictures [Translate 650 400 $ scale 2.5 2.5 $ fromJust $ lookup "iconeHome" ts,
                               Translate 770 400 $ scale 4 3.5 $ fromJust $ lookup "iconePausa" ts,
                               Translate 770 400 $ scale 0.5 0.5 $ fromJust $ lookup "voltarAoMenu" ts  
                              ]
         iconeJogador = Pictures [Translate 760 300 $ scale 5.5 5.5  $ fromJust $ lookup "iconeLoja" ts, 
                                  Translate 760 300 $ scale 1.5 1.5 $ fromJust $ lookup "player" ts
                                 ]
         nInimigos = Pictures [Translate 670 40 $ scale 3.5 3.5 $ fromJust $ lookup "nInimigos" ts, 
                               Translate 780 40 $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts, 
                               Translate 750 55 $ scale 1 1 $ string2FonteNumeros (show $ length (inimigosJogo j)) ts 
                              ]
         modoJogo1 = Pictures [Translate 670 (-20) $ scale 3.5 3.5 $ fromJust $ lookup "botaomodo" ts, 
                              Translate 780 (-20) $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts 
                             ]
         nivelJogador = Pictures [Translate 670 (-85) $ scale 3.5 3.5 $ fromJust $ lookup "botaoNivel" ts, 
                                  Translate 780 (-85) $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts, 
                                  desenhaNivelJogo it ts
                                 ]
         multiplayer1 = Pictures [if (multiplayer it) then Translate 670 (-150) $ scale 1.2 1.2 $ fromJust $ lookup "cadeadoAberto" ts 
                                   else Translate 670 (-150) $ scale 1.2 1.2 $  fromJust $ lookup "cadeadoFechado" ts,  
                                   Translate 780 (-150) $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts, 
                                   Translate 780 (-150) $ scale 0.5 0.5 $ fromJust $ lookup "multiplayer" ts
                                  ]

desenhaModoJogo :: ImmutableTowers -> [Textura] -> Picture
desenhaModoJogo it ts = Translate 780 (-20) $ scale 0.8 0.8 $ 
      case modoJogo it of
        Infinito -> fromJust $ lookup "infinito" ts 
        Finito -> fromJust $ lookup "finito" ts 
        _      -> fromJust $ lookup "criarMapa" ts

desenhaNivelJogo :: ImmutableTowers -> [Textura] -> Picture 
desenhaNivelJogo it ts = case modoJogo it of 
    Finito   -> Translate 780 (-80) $ scale 0.8 0.8 $ desenhaNivel it ts
    Infinito -> Translate 750 (-70) $ string2FonteNumeros (show $ nivelJogoInfinito it) ts
    _        -> Translate 775 (-85) $ fromJust $ lookup "semNivel" ts
  where desenhaNivel :: ImmutableTowers -> [Textura] -> Picture
        desenhaNivel it_ texturas = case nivelJogoFinito it_ of 
            Nivel1 -> fromJust $ lookup "Nivel1" texturas
            Nivel2 -> fromJust $ lookup "Nivel2" texturas
            Nivel3 -> fromJust $ lookup "Nivel3" texturas
            Nivel4 -> fromJust $ lookup "Nivel4" texturas
            Nivel5 -> fromJust $ lookup "Nivel5" texturas

desenhaPausa :: ImmutableTowers -> Picture 
desenhaPausa it = Pictures [fundo, 
                             iconeBackToMenu, 
                             iconeRestart, 
                             iconeNextLevel, 
                             frasePaused, 
                             fraseBackToMenu, 
                             fraseRestart,
                             fraseContinuar,
                             seta
                            ]
    where fundo = color (withAlpha 0.8 black) $ rectangleSolid 1920 1080 
          frasePaused = scale 0.8 0.8 $ fromJust $ lookup "frasePaused" ts
          fraseBackToMenu =  Translate (-350) (-250) $ scale 0.6 0.6 $ fromJust $ lookup "fraseBackToMenu" ts
          fraseContinuar = Translate (0) (-250) $ scale 1.2 1.2 $ fromJust $ lookup "fraseContinuar" ts 
          fraseRestart = Translate (350) (-250) $ scale 0.7 0.7 $ fromJust $ lookup "fraseRestart" ts
          (x,y) = botaoNivelPassado it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeNextLevel = Translate (0) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate (350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          ts = texturasIT it

desenhaTutorial :: ImmutableTowers -> [Textura] -> Picture
desenhaTutorial it ts = case estadoIT it of
    Tutorial -> Pictures [desenhaJogo it, desenhaEtapasTT it ts] 
    TutorialEscolhendoTorre -> Pictures [desenhaJogo it, desenhaEscolhendoTorre it, desenhaEtapasTT it ts] 
    TutorialComprando -> Pictures [desenhaJogo it, desenhaComprando it, desenhaEtapasTT it ts]

desenhaEtapasTT :: ImmutableTowers -> [Textura] -> Picture
desenhaEtapasTT it ts = case etapaTT it of 
    0 -> fromJust $ lookup "tutorial1" ts  
    1 -> fromJust $ lookup "tutorial2" ts 
    2 -> fromJust $ lookup "tutorial3" ts
    3 -> Pictures [fromJust $ lookup "fundoPedraTT" ts,
                   Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts,
                   Translate (-350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts,
                   Translate 350 (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts, 
                   Translate (-350) (-250) $ scale 1.2 1.2 $ fromJust $ lookup "no" ts,
                   Translate 350 (-250) $ scale 1.2 1.2 $ fromJust $ lookup "yes" ts
                   ]
    4 -> Translate 750 (-250) $ fromJust $ lookup "etapa1" ts
    5 -> Translate 750 (-250) $ scale 0.7 0.7 $ fromJust $ lookup "etapa2" ts
    6 -> Translate 750 (-250) $ scale 0.6 0.6 $ fromJust $ lookup "etapa3" ts
    7 -> Translate 750 (-250) $ scale 0.5 0.5 $ fromJust $ lookup "etapa4" ts
  where (x,y) = botaoGameOver it
             