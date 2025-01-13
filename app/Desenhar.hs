module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Data.Maybe (fromJust)
import GHC.Float (int2Float)
import Tarefa2 

{-| Dimensão de cada terreno. 

-}

lado :: Integer
lado = 64

{-| Altura do mapa. 

-}

altura :: Integer
altura = 64*16

{-| Comprimento do mapa. 

-}

comprimento :: Integer
comprimento = 64*16

{-| Função principal que desenha o estado atual do jogo. 

-}

desenha :: ImmutableTowers -> Picture
desenha it = Pictures [desenhaPrimeiroJogador it, desenhaSegundoJogador it]

{-| Desenha o estado do jogo para o primeiro jogador. 

-}

desenhaPrimeiroJogador :: ImmutableTowers -> Picture
desenhaPrimeiroJogador it = case estadoIT it of
     Menu -> desenhaMenu  it 
     EscolhendoTorre -> Pictures [desenhaEscolhendoTorre it]
     Comprando -> desenhaComprando it
     Pausado -> Pictures [desenhaJogo it, 
                          desenhaPausa it]  
     CriandoMapa -> desenhaCriandoMapa it
     EscolhendoOndas -> desenhaEscolhendoOnda it 
     EscolhendoIG -> desenhaEscolhendoOnda it 
     EscolhendoIM -> desenhaEscolhendoOnda it
     NivelPassado -> Pictures [desenhaJogo it, 
                               desenhaNivelPassado it]
     GameOver -> Pictures [desenhaJogo it, 
                           desenhaGameOver it ]
     YouWon -> Pictures [desenhaJogo it, desenhaYouWon it]
     MensagemErro -> if modoJogo it == MapaCriado 
                     then Pictures [desenhaCriandoMapa it, 
                                    desenhaMensagemErro it] 
                     else Pictures [desenhaMenu it, 
                                    desenhaMensagemErro it]
     YouWonCM -> Pictures [desenhaJogo it, 
                           desenhaYouWonCM it]
     Tutorial -> desenhaTutorial it ts
     TutorialEscolhendoTorre -> desenhaTutorial it ts 
     TutorialComprando -> desenhaTutorial it ts 
     Jogando -> Pictures [desenhaJogo it] 
     Costumizar -> desenhaCostumizar it ts
     _ -> Pictures []
     
  where ts = texturasIT it

{-| Desenha o estado do jogo para o segundo jogador. 

-}

desenhaSegundoJogador :: ImmutableTowers -> Picture
desenhaSegundoJogador it = if (estadoIT it == Jogando || estadoIT it == EscolhendoTorre || estadoIT it == Comprando) then 
    case estadoIT2 it of 
        EscolhendoTorre2 -> Pictures [desenhaEscolhendoTorre2 it]
        Comprando2 -> Pictures [desenhaComprando2 it]
        _  -> Pictures []
 else Pictures []

{-| Converte uma String composta por caracteres de números em uma imagem. 
-}

string2FonteNumeros :: String -> [Textura] -> Picture
string2FonteNumeros s ts = Pictures $ auxString2FonteNumeros s ts 0
   where auxString2FonteNumeros :: String -> [Textura] -> Float -> [Picture]
         auxString2FonteNumeros [] _ _ = []
         auxString2FonteNumeros (h:t) ts ac = 
            let resultado = lookup ("numero" ++ [h]) ts
                espacamento = 13
            in
                 case resultado of
                    Nothing -> [translate (ac*espacamento) 0 $ fromJust $ lookup "numero0" ts]
                    _ -> (translate (ac*espacamento) 0 $ fromJust resultado) : auxString2FonteNumeros t ts (ac+1)
             
{-| Converte coordenadas para o sistema de coordenadas do Gloss. 

-}
conversaoCoordsGloss :: Posicao -> Posicao
conversaoCoordsGloss (x,y) = ((x*64) - (7.5*64), (7.5*64) - (y*64))

{-| Desenha um tipo de terreno com base nas suas texturas. 

-}

desenhaTerreno :: [Textura] -> Terreno -> Picture
desenhaTerreno ts Terra = fromJust $ lookup "terra" ts
desenhaTerreno ts Relva = fromJust $ lookup "relva" ts
desenhaTerreno ts Agua = fromJust $ lookup "agua" ts 

{-| Responsável por desenhar os mapas do jogo, convertendo cada terreno em uma imagem correspondente. 

-}

desenhaMapa :: Mapa -> [Textura] -> Picture
desenhaMapa mapa textures =
    let t = getMapaTexturas mapa textures
    in Pictures [fromJust $ lookup "fundoJogo" textures, 
                 pictures [translate ((fromInteger x * fromInteger lado )-7.5*64) ((fromInteger y * fromInteger lado) +7.5*64 ) ((t!!abs (fromInteger y))!!fromInteger x) | (x,y) <- positions]]

{-| Lista de posições utilizadas para desenhar o mapa no Gloss. 

-} 

positions :: [(Integer,Integer)]
positions = [(x,y) | y <- [0,(-1)..(-15)], x <- [0..15]]

{-| Converte mapa em uma matriz de Picture. 

-}

getMapaTexturas :: Mapa -> [Textura] -> [[Picture]]
getMapaTexturas [] _ = []
getMapaTexturas (h:t) textures = map (textures `desenhaTerreno`) h : getMapaTexturas t textures

{-| Desenha a base do jogo. 

==__Nota:__ 
(104-64)/2 -> desenha no mapa tendo em conta a altura da textura 

-}

desenhaBase :: Base -> Picture -> Picture
desenhaBase base textura =
    let (x,y) = conversaoCoordsGloss $ posicaoBase base
    in translate x (y  + (104-64)/2) textura  

{-| Desenha os inimigos do jogo. 

-}

desenhaInimigos :: [Inimigo] -> ImmutableTowers -> [Textura] -> [Picture]
desenhaInimigos [] _ _ = []
desenhaInimigos (i:is) it texturas = desenhaUmInimigo i it texturas : desenhaInimigos is it texturas

{-| Desenha um único inimigo, associado a sua vida e aos projéteis que atuam sobre ele. 

-}

desenhaUmInimigo :: Inimigo -> ImmutableTowers -> [Textura] -> Picture 
desenhaUmInimigo inimigo it texturas = 
    let (x, y) = conversaoCoordsGloss $ posicaoInimigo inimigo
        comprimentoNumeroVidaPxs = int2Float (length (show $ ceiling $ vidaInimigo inimigo) * 13)
        offsetNumeroVida = (comprimentoNumeroVidaPxs+27+18)/2*0.5 -- metade do comprimento da vida, da largura do inimigo e da largura do coração, escalado a 0.5
        numeroDaVida = translate (x-offsetNumeroVida) (y+40) $ scale 0.5 0.5 $ string2FonteNumeros (show $ ceiling $ vidaInimigo inimigo) texturas
        coracaoVida = translate (x+offsetNumeroVida) (y+40-(16/2*0.7)) $ scale 0.7 0.7 $ fromJust $ lookup "vida" texturas
        textura = desenhaAnimacaoInimigo inimigo it texturas
        indicativoProjeteis = Translate x (y + 50) $ desenhaIndicativoProjeteis inimigo texturas
    in Pictures [translate x y textura, 
                 numeroDaVida, 
                 coracaoVida,
                 indicativoProjeteis
                ]
                 

{-| Desenha um indicativo de projéteis ativos nos inimigos. 

-}

desenhaIndicativoProjeteis :: Inimigo -> [Textura] -> Picture
desenhaIndicativoProjeteis i ts
    | elem Gelo tiposProjs && elem Resina tiposProjs && elem Fogo tiposProjs = Pictures [Translate (-30) 0 iGelo, 
                                                                                         iResina, 
                                                                                         Translate 30 0 iFogo]
    | elem Gelo tiposProjs && elem Resina tiposProjs = Pictures [Translate (-10) 0 iGelo, 
                                                                 Translate 10 0 iResina]
    | elem Gelo tiposProjs && elem Fogo tiposProjs = Pictures [Translate (-10) 0 iGelo, 
                                                               Translate 10 0 iFogo]
    | elem Fogo tiposProjs && elem Resina tiposProjs = Pictures [Translate (-10) 0 iFogo, 
                                                                 Translate 10 0 iResina]
    | elem Gelo tiposProjs = Pictures [iGelo]
    | elem Fogo tiposProjs = Pictures [iFogo]
    | elem Resina tiposProjs = Pictures [iResina]
    | otherwise = Pictures []
    where tiposProjs = getTiposProjsInimigo i
          iGelo = fromJust $ lookup "indicativoProjetilGelo" ts
          iFogo = fromJust $ lookup "indicativoProjetilFogo" ts
          iResina = fromJust $ lookup "indicativoProjetilResina" ts

{-| Permite animar o inimigo. Sendo possível visualizá-lo em movimento.

==__Nota:__
Desenha cada imagem associada ao inimigo durante 4 frames. 

-}

desenhaAnimacaoInimigo :: Inimigo -> ImmutableTowers -> [Textura] -> Picture
desenhaAnimacaoInimigo i it ts =
    let its = iteracoesDesdeInicioAnimacaoInimigo i
        textura = case tipoInimigo i of
            Homem -> fromJust $ lookup (inimigoHomem it ++ show (ceiling $ int2Float(its) / 4)) ts
            Mulher -> fromJust $ lookup (inimigoMulher it ++ show (ceiling $ int2Float(its) / 4)) ts
    in  Pictures [textura]

{-| Desenha os portais do jogo. 

==__Nota:__ 
(128-64)/2 -> desenha no mapa tendo em conta a altura da textura 

-}

desenhaPortais :: [Portal] -> Picture -> [Picture]
desenhaPortais [] _ = []
desenhaPortais (p:ps) textura = 
    let (x,y) = conversaoCoordsGloss $ posicaoPortal p
    in translate x (y  + (128-64)/2) textura : desenhaPortais ps textura

{-| Desenha as torres que são adicionadas pelo jogador no mapa. 

-}

desenhaTorres :: [Torre] -> [Textura] -> Picture 
desenhaTorres torres texturas = Pictures $ map (`desenhaUmaTorre` texturas) torres 

{-| Desenha uma torre, de acordo com a textura associada. 

-}

desenhaUmaTorre :: Torre -> [Textura] -> Picture
desenhaUmaTorre torre texturas = 
    let (x,y) = conversaoCoordsGloss $ posicaoTorre torre 
        textura = case tipoProjetil (projetilTorre torre) of 
            Gelo -> fromJust $ lookup "torreGelo" texturas
            Resina -> fromJust $ lookup "torreResina" texturas
            Fogo -> fromJust $ lookup "torreFogo" texturas
        texturaAnimacao = desenhaAnimacaoTorre torre texturas
    in Pictures [translate x (y + (121-64)/2) textura, texturaAnimacao]

{-| Desenha os projéteis sendo lançados quando as torres estão preparadas e quando há inimigos por perto.

-}

desenhaAnimacaoTorre :: Torre -> [Textura] -> Picture
desenhaAnimacaoTorre t ts =
    let (x,y) = conversaoCoordsGloss $ posicaoTorre t
        iteracoes = iteracoesDesdeInicioAnimacao t
        textura = case tipoProjetil (projetilTorre t) of
            Gelo -> fromJust $ lookup ("animacaoTorreGelo" ++ show iteracoes) ts
            Resina -> fromJust $ lookup ("animacaoTorreResina" ++ show iteracoes) ts
            Fogo -> fromJust $ lookup ("animacaoTorreFogo" ++ show iteracoes) ts
    in translate x (y+80) textura

{-| Desenha o alcance de cada torre. 

-}

desenhaAlcanceTorres :: [Torre] -> [Picture]
desenhaAlcanceTorres [] = []
desenhaAlcanceTorres (t:ts) = 
    let (x,y) = conversaoCoordsGloss $ posicaoTorre t
        alcance = case tipoProjetil (projetilTorre t) of 
            Gelo -> Translate x y $ color (makeColorI 72 99 100 100) $ circleSolid ((alcanceTorre t)*64)
            Fogo -> Translate x y $ color (makeColorI 87 41 22 100) $ circleSolid ((alcanceTorre t)*64)
            _ -> Translate x y $ color (makeColorI 97 64 55 100) $ circleSolid ((alcanceTorre t)*64)
    in alcance : desenhaAlcanceTorres ts

{-| É responsável por renderizar todos os elementos do jogo. 

== __Elementos desenhados:__
    
    1. Mapa do jogo.
    2. Moldura superior e inferior.
    3. Base do jogador.
    4. Inimigos presentes no jogo.
    5. Torres posicionadas no mapa.
    6. Portais para lançar inimigos.
    7. Loja para compra de torres.
    8. Perfil do jogador.
-}


desenhaJogo :: ImmutableTowers -> Picture
desenhaJogo it = Pictures [picMapa, 
                           picAlcanceTorres,
                           picFundoMapa,
                           picMolduraMapa, 
                           picInimigo,
                           picTorre,
                           Pictures picPortais, 
                           picLoja, 
                           picBase, 
                           creditosJog, 
                           moldBaixo
                          ]
    where picMapa = desenhaMapa mapa texturas
          jogo = jogoIT it
          mapa = mapaJogo jogo
          picMolduraMapa = fromJust $ lookup "molduraMapa" texturas
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
          picFundoMapa = fromJust $ lookup "fundoMapa" texturas
          picAlcanceTorres = Pictures $ desenhaAlcanceTorres torres

{-| Responsável por renderizar a tela inicial do jogo, onde o jogador pode acessar diferentes modos de jogo. 

-}

desenhaMenu :: ImmutableTowers -> Picture 
desenhaMenu it = Pictures 
    [fundoTela,
    translate 0 250 $ scale 1 1 itTitulo, 
    scale 5 5 botaoMenu, 
    fm, 
    translate 0 (-100) $ scale 5 5 botaoMenu,
    translate 0 (-100) im, 
    translate 0 (-200) $ scale 5 5 botaoMenu,
    translate 0 (-200) cm, 
    translate 0 (-300) $ scale 5 5 botaoMenu,
    translate 0 (-300) $ scale 1.2 1.2 tt,
    translate 0 (-400) $ scale 5 5 botaoMenu,
    translate 0 (-400) $ scale 1.2 1.2 costumize,
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
    
{-| É responsável por renderizar a interface de criação de mapa no jogo. 

-}

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
    fundo = color (withAlpha 0.5 black) $ rectangleSolid 1024 1024 
    modo = Translate 780 (-20) $ scale 0.8 0.8 $ fromJust $ lookup "criarMapa" ts

{-| Renderiza todos os terrenos definidos no mapa.

-} 

desenhaListaTerreno :: [(Posicao, Terreno)] -> [Textura] -> Picture
desenhaListaTerreno lt ts = Pictures $ map (`desenhaUMterreno` ts) lt 

{-| Renderiza um único terreno em uma posição específica.

-}

desenhaUMterreno :: (Posicao, Terreno) -> [Textura] -> Picture 
desenhaUMterreno ((x,y), terreno) ts = 
    let (x1,y1) = conversaoCoordsGloss (x,y)
    in Translate x1 y1 $ desenhaTerreno ts terreno

{-| Desenha o estado de quando o jogador está a escolher os parâmetros dos portais, quando 'modoJogo' == MapaCriado 

-}

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
                                 Translate (-10) 0 setaLadoL,
                                 Translate 250 0 setaLado,
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
                                 Translate 240 0 setaLadoL,
                                 enO, 
                                 enIG, 
                                 enIM]
    _ -> Pictures []
    where 
        ts = texturasIT it 
        (nO, nIG, nIM) = escolhendoParametros it
        enO = translate (-135) 25 $ scale 1.5 1.5 $ string2FonteNumeros (show nO) ts
        enIG = translate (-35) 25 $ scale 1.5 1.5 $ string2FonteNumeros (show nIG) ts
        enIM = translate 58 25 $ scale 1.5 1.5 $ string2FonteNumeros (show nIM) ts
        botao1 = translate (-100) 0 $ scale 5 5 $ fromJust $ lookup "botaoQ" ts
        botao2 = scale 5 5 $ fromJust $ lookup "botaoQ" ts
        botao3 = translate 100 0 $ scale 5 5 $ fromJust $ lookup "botaoQ" ts
        setaCima1 = translate 100 90 $ scale 4 4 $ fromJust $ lookup "setaCima" ts
        setaCima2 = translate (-5) 90 $ scale 4 4 $ fromJust $ lookup "setaCima" ts
        setaCima3 = translate (-104) 90 $ scale 4 4 $ fromJust $ lookup "setaCima" ts
        setaBaixo1 = translate 100 (-90) $ scale 4 4 $ fromJust $ lookup "setaBaixo" ts
        setaBaixo2 = translate (-5) (-90) $ scale 4 4 $ fromJust $ lookup "setaBaixo" ts
        setaBaixo3 = translate (-105) (-90) $ scale 4 4 $ fromJust $ lookup "setaBaixo" ts
        nOndas = translate (-250) (-180) $ scale 0.7 0.7 $ fromJust $ lookup "nOndas" ts
        ninimigoM = translate 0 (-180) $ scale 0.5 0.5 $ fromJust $ lookup "ninimigoM" ts 
        ninimigoF = translate 250 (-180) $ scale 0.5 0.5 $ fromJust $ lookup "ninimigoF" ts
        setaLado = translate (-120) (-180) $ scale 3 3 $ fromJust $ lookup "seta" ts
        setaLadoL = translate (-120) (-180) $ scale 3 3 $ rotate 180 $ fromJust $ lookup "seta" ts
        fundo = Pictures [desenhaCriandoMapa it, color (withAlpha 0.8 black) $ rectangleSolid 1920 1080]

{-| Responsável por desenhar a loja, onda o jogador pode comprar as torres. 

-}

desenhaLoja :: Loja -> [Textura] -> Picture
desenhaLoja loja ts = Pictures [iconeLoja, 
                                store, 
                                fundoTorre1, 
                                fundoTorre2, 
                                fundoTorre3, 
                                Pictures $ map desenhaTorre loja,
                                fogo, 
                                gelo, 
                                resina]
    where x = -805
          y = 100
          espacamento = 200
          tamanhoTorre = 0.70
          tamanhoCreditos = 1
          iconeLoja = Translate (-750) 270 $ scale 5.5 5.5  $ fromJust $ lookup "iconeLoja" ts
          gelo = Translate (-730) 120 $ scale 4 4 $ fromJust $ lookup "gelo" ts
          resina = Translate (-730) (-80) $ scale 4 4 $ fromJust $ lookup "resina" ts
          fogo = Translate (-730) (-280) $ scale 4 4 $ fromJust $ lookup "fogo" ts
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

{-| Desenha um esstado do jogo, em que o jogador 2 escolhe o local que vai posicionar a torre que esta sendo comprada. 

-}

desenhaComprando2 :: ImmutableTowers -> Picture 
desenhaComprando2 it = Pictures [desenhaSelecaoSndJog selec2]
  where
    selec2 = posicaoSelecionadaMapaSndJog it 

{-| Desenha um esstado do jogo, em que o jogador escolhe o local que vai posicionar a torre que esta sendo comprada. 

-}

desenhaComprando :: ImmutableTowers -> Picture 
desenhaComprando it = Pictures [desenhaJogo it, 
                                desenhaSelecao selec]
  where
    selec = posicaoSelecionadaMapa it 

{-| Desenha uma seta, que mode ser movida. 

-}

desenhaSelecaoSeta :: (Float, Float) -> [Textura] -> Picture
desenhaSelecaoSeta (x,y) ts = Pictures [Translate x y $ scale 3 3 $ fromJust $ lookup "seta" ts]

{-| Desenha uma seta para a indicar o botão que está a ser escolhido no menu pelo jogador. 

-}

desenhaEscolhendoMenu :: ImmutableTowers -> Picture 
desenhaEscolhendoMenu it = Pictures [desenhaSelecaoSeta selec ts]
  where selec = botaoMenu it 
        ts = texturasIT it

{-| Desenha uma seta para a indicar a torre que está a ser escolhida pelo jogador. 

-}

desenhaEscolhendoTorre :: ImmutableTowers -> Picture 
desenhaEscolhendoTorre it = Pictures [desenhaJogo it, desenhaSelecaoSeta selec ts]
  where selec = produtoLoja it 
        ts = texturasIT it

{-| Desenha uma seta para a indicar a torre que está a ser escolhida pelo jogador 2. 

-}

desenhaEscolhendoTorre2 :: ImmutableTowers -> Picture 
desenhaEscolhendoTorre2 it = Pictures [desenhaSelecaoSeta selec2 ts]
  where 
        selec2 = produtoLoja2 it 
        ts = texturasIT it

{-| Responsável por desenhar a seleção vermelha no mapa

-}

desenhaSelecao :: (Float, Float) -> Picture
desenhaSelecao pos =
    let (x,y) = conversaoCoordsGloss pos
    in translate x y $
       color (withAlpha 0.5 red) $ rectangleSolid 64 64

{-| Responsável por desenhar a seleção azul no mapa

-}

desenhaSelecaoSndJog :: (Float, Float) -> Picture
desenhaSelecaoSndJog pos =
    let (x,y) = conversaoCoordsGloss pos
    in translate x y $
       color (withAlpha 0.5 blue) $ rectangleSolid 64 64

{-| Desenha o perfil do jogador, nos da indicação dos seguintes elementos: 

1. Vida da base 
2. Créditos que o jogador tem disponível. 
3. Nível que o jogador se encontra. 
4. Modo que o jogador está a jogar. 
5. Se o 2º Jogador esta ativado ou desativado. 

-}

desenhaPerfilJogador :: ImmutableTowers -> [Textura] -> Picture 
desenhaPerfilJogador it ts  
    | (modoJogo it == MapaCriado) || (estadoIT it == Tutorial) || (estadoIT it == TutorialEscolhendoTorre) || (estadoIT it == TutorialComprando)
               =  Pictures [creditosJogador,
                            perfilJogador, 
                            creditos, 
                            iconeVida, 
                            vidaBaseJg, 
                            iconePausa, 
                            iconeHome, 
                            iconeJogador, 
                            nInimigos, 
                            nivelJogador, 
                            modoJogo1
                            ]
    | otherwise         = Pictures [creditosJogador,
                            perfilJogador, 
                            creditos, 
                            iconeVida, 
                            vidaBaseJg, 
                            iconePausa, 
                            iconeHome, 
                            iconeJogador, 
                            nInimigos, 
                            nivelJogador, 
                            modoJogo1,
                            multiplayer1
                            ]
 where desenhaModoJogo :: ImmutableTowers -> [Textura] -> Picture
       desenhaModoJogo it' ts' = Translate 780 (-20) $ scale 0.8 0.8 $ 
            case modoJogo it' of
               Infinito -> fromJust $ lookup "infinito" ts' 
               Finito -> fromJust $ lookup "finito" ts' 
               _      -> fromJust $ lookup "criarMapa" ts'
       j = jogoIT it 
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
                              Translate 780 (-20) $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts,
                              desenhaModoJogo it ts 
                             ]
       nivelJogador = Pictures [Translate 670 (-85) $ scale 3.5 3.5 $ fromJust $ lookup "botaoNivel" ts, 
                                  Translate 780 (-85) $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts, 
                                  desenhaNivelJogo it ts
                                 ]
       multiplayer1 = Pictures [if multiplayer it then Translate 670 (-150) $ scale 1.2 1.2 $ fromJust $ lookup "cadeadoAberto" ts 
                                   else Translate 670 (-150) $ scale 1.2 1.2 $  fromJust $ lookup "cadeadoFechado" ts,  
                                   Translate 780 (-150) $ scale 3.5 3.5 $ fromJust $ lookup "iconePausa" ts, 
                                   Translate 780 (-150) $ scale 0.5 0.5 $ fromJust $ lookup "multiplayer" ts
                                  ]

{-| Desenha o nível em que o jogador se encontra.

-}

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

{-| Desenha o estado da pausa. 

-}

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
          fraseContinuar = Translate 0 (-250) $ scale 1.2 1.2 $ fromJust $ lookup "fraseContinuar" ts 
          fraseRestart = Translate 350 (-250) $ scale 0.7 0.7 $ fromJust $ lookup "fraseRestart" ts
          (x,y) = botaoNivelPassado it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeNextLevel = Translate 0 (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate 350 (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          ts = texturasIT it

{-| Desenha o tutorial do jogo. 

-}

desenhaTutorial :: ImmutableTowers -> [Textura] -> Picture
desenhaTutorial it ts = case estadoIT it of
    Tutorial -> Pictures [desenhaJogo it, desenhaEtapasTT it ts] 
    TutorialEscolhendoTorre -> Pictures [desenhaJogo it, desenhaEscolhendoTorre it, desenhaEtapasTT it ts] 
    TutorialComprando -> Pictures [desenhaJogo it, desenhaComprando it, desenhaEtapasTT it ts]
    _ -> Pictures []

{-| Desenha as etapas do tutorial. Sempre que o inimigo avança uma etapa, a função atualiza a interface gráfica.  

-}

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
    _ -> Pictures []
  where (x,y) = botaoGameOver it

{-| Desenha um estado onde o jogadorpode escolher mudar os personagens do jogo. 

-}

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
        Translate 650 (-450) $ fromJust $ lookup "costumizarVoltar" ts
    ]
    where (x,y) = selecaoCostumizar it

{-| Desenha a tela de quando o jogador vence um nível. 

-}

desenhaNivelPassado :: ImmutableTowers -> Picture
desenhaNivelPassado it = Pictures [fundo, iconeBackToMenu, iconeNextLevel, iconeRestart, fraseLevelWon, fraseBackToMenu, fraseNextLevel, fraseRestart, seta]
    where ts = texturasIT it
          fundo = color (withAlpha 0.7 orange) $ rectangleSolid 1920 1080 
          fraseLevelWon = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseLevelWon" ts
          fraseBackToMenu =  Translate (-350) (-250) $ scale 0.7 0.7 $ fromJust $ lookup "fraseBackToMenu" ts
          fraseNextLevel = Translate 0 (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseNextLevel" ts 
          fraseRestart = Translate 350 (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseRestart" ts
          (x,y) = botaoNivelPassado it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeNextLevel = Translate 0 (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate 350 (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts
           
{-| Desenha a tela de quando o jogador perde. 

-}

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
          fundo = color (withAlpha 0.8 red) $ rectangleSolid 1920 1080 
          fraseGameOver = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseGameOver" ts
          fraseBackToMenu =  Translate (-350) (-250) $ fromJust $ lookup "fraseBackToMenu" ts
          fraseRestart = Translate 350 (-250) $ fromJust $ lookup "fraseRestart" ts 
          (x,y) = botaoGameOver it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate 350 (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 

{-| Desenha a tela de quando o jogador ganha, quando o 'modoJogo' == MapaCriado

-}

desenhaYouWonCM :: ImmutableTowers -> Picture
desenhaYouWonCM it = Pictures [fundo, 
                               iconeBackToMenu, 
                               iconeRestart, 
                               fraseGameOver, 
                               fraseBackToMenu, 
                               fraseRestart, 
                               seta
                              ]
    where ts = texturasIT it
          fundo = color (withAlpha 0.8 orange) $ rectangleSolid 1920 1080 
          fraseGameOver = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseYouWon" ts
          fraseBackToMenu =  Translate (-350) (-250) $ fromJust $ lookup "fraseBackToMenu" ts
          fraseRestart = Translate 350 (-250) $ fromJust $ lookup "fraseRestart" ts 
          (x,y) = botaoGameOver it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate 350 (-250) $ scale 8 8 $ fromJust $ lookup "iconePausa" ts 

{-| Desenha a tela de quando o jogador ganha nos outros modos. 

-}

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
          fundo = color (withAlpha 0.8 orange) $ rectangleSolid 1920 1080 
          fraseYouWon = Translate 0 200 $ scale 0.8 0.8 $ fromJust $ lookup "fraseYouWon" ts
          fraseBackToMenu =  Translate (-350) (-250) $ scale 0.7 0.7 $ fromJust $ lookup "fraseBackToMenu" ts
          fraseNextLevel = Translate 0 (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseNextLevel" ts 
          fraseRestart = Translate 350 (-250) $ scale 0.8 0.8 $ fromJust $ lookup "fraseRestart" ts
          (x,y) = botaoNivelPassado it
          seta = Translate x y $ scale 4 4 $ fromJust $ lookup "seta" ts
          iconeBackToMenu = Translate (-350) (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeNextLevel = Translate 0 (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 
          iconeRestart = Translate 350 (-250) $ scale 5 5 $ fromJust $ lookup "iconePausa" ts 

{-| Desenha uma mensagem de erro, quando o jogo está inválido. 

-}

desenhaMensagemErro :: ImmutableTowers -> Picture
desenhaMensagemErro it = Pictures [fromJust $ lookup "mensagemErro" (texturasIT it), 
                                   color (withAlpha 0.5 black) $ rectangleSolid 1920 1080]