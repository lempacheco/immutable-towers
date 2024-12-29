module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425

itInicial :: [Picture] -> ImmutableTowers
itInicial texturas = ImmutableTowers { 
  jogoIT = Jogo {
        baseJogo = Base {
          vidaBase = 50,
          posicaoBase = (7.5*64,-(1.5*64)),
          creditosBase = 0
          },
        torresJogo = [Torre {posicaoTorre = (-5.5*64, -5.5*64), projetilTorre = Projetil {tipoProjetil = Gelo,duracaoProjetil = Finita 10}, danoTorre = 4,
                             alcanceTorre = 5,
                             rajadaTorre = 3,
                             cicloTorre = 2,
                             tempoTorre = 0}, 
                      Torre {posicaoTorre = (-4.5*64, 4.5*64), projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, danoTorre = 4,
                             alcanceTorre = 5,
                             rajadaTorre = 3,
                             cicloTorre = 2,
                             tempoTorre = 0}, 
                      Torre {posicaoTorre = (6.5*64, 0.5*64), projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}, danoTorre = 4,
                             alcanceTorre = 5,
                             rajadaTorre = 3,
                             cicloTorre = 2,
                             tempoTorre = 0}],
        portaisJogo = [
                        Portal {posicaoPortal = (-(7.5*64),-(1.5*64)), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (10.5,10.5), tipoInimigo = GuerreiroFogo, projeteisInimigo = [], vidaInimigo = 6, butimInimigo = 4, direcaoInimigo = Este, ataqueInimigo = 5, velocidadeInimigo = 7}, Inimigo {posicaoInimigo = (1.5,1.5), tipoInimigo = MulherLanca, projeteisInimigo = [], vidaInimigo = 6, butimInimigo = 4, direcaoInimigo = Este, ataqueInimigo = 5, velocidadeInimigo = 7}],
                                cicloOnda = 2*60,
                                tempoOnda = 0,
                                entradaOnda = 0}]}
                      --  Portal {posicaoPortal = (-(2.5*64),7.5*64)}
                      ],
        mapaJogo = mapaInicial,
        inimigosJogo = [{- Inimigo {posicaoInimigo = (10.5,10.5), tipoInimigo = GuerreiroFogo}, Inimigo {posicaoInimigo = (1.5,1.5), tipoInimigo = MulherLanca} -}],
        lojaJogo = []
      },
  texturasIT = texturas
}

mapaInicial :: Mapa 
mapaInicial = 
  [ [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,t,t,t,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,r,r,t,r,r,r,a,a,r,r,r],
    [r,r,r,r,r,r,r,t,r,r,r,a,a,r,r,r],
    [r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,r,r,r,a,a,a,r,r,t],
    [t,t,t,r,r,r,t,r,r,a,a,a,r,t,t,t],
    [r,r,r,r,r,r,t,r,r,a,a,r,r,t,r,r],
    [r,r,r,r,r,r,t,t,t,t,t,t,t,t,t,t],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r],
    [r,r,r,r,r,r,r,r,a,a,a,a,r,r,r,r]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

janela :: Display
janela = {- InWindow "Immutable Towers" (fromInteger comprimento, fromInteger altura) (0, 0) -} FullScreen

fundo :: Color
fundo = white

fr :: Int
fr = 60

main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"
  terra <- loadBMP "resources/textures/map/Terra.bmp"
  agua <- loadBMP "resources/textures/map/Agua.bmp"
  relva <- loadBMP "resources/textures/map/Relva.bmp"
  torreGelo <- loadBMP "resources/textures/towers/TorreGelo.bmp"
  torreResina <- loadBMP "resources/textures/towers/TorreResina.bmp"
  torreFogo <- loadBMP "resources/textures/towers/TorreFogo.bmp"
  base <- loadBMP "resources/textures/base/Base.bmp"
  portal <- loadBMP "resources/textures/portal/Portal.bmp"
  guerreiroFogo <- loadBMP "resources/textures/entities/GuerreiroFogo.bmp"
  mulherLanca <- loadBMP "resources/textures/entities/MulherLanca.bmp"
  play janela 
        fundo 
        fr 
        (itInicial 
          [
            terra,         --0; 64x64 px
            relva,         --1; 64x64 px
            agua,          --2; 64x64 px
            torreGelo,     --3; 64x106 px
            torreResina,   --4; 64x114 px
            torreFogo,     --5; 64x121 px
            base,          --6; 64x104 px
            portal,        --7; 64x128 px
            guerreiroFogo, --8; 27x47 px
            mulherLanca    --9; 27x50 px
          ]
        ) 
        desenha 
        reageEventos 
        reageTempo