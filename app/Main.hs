
module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425

itInicial :: [(String, Picture)] -> ImmutableTowers
itInicial = it1

janela :: Display
janela = {-InWindow "Immutable Towers" (fromInteger comprimento, fromInteger altura) (0, 0)-} FullScreen

fundo :: Color
fundo = makeColorI 20 60 85 100

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
  animacaoTorreGelo1 <- loadBMP "resources/textures/towers/animacoesTorreGelo/1.bmp"
  animacaoTorreGelo2 <- loadBMP "resources/textures/towers/animacoesTorreGelo/2.bmp"
  animacaoTorreGelo3 <- loadBMP "resources/textures/towers/animacoesTorreGelo/3.bmp"
  animacaoTorreGelo4 <- loadBMP "resources/textures/towers/animacoesTorreGelo/4.bmp"
  animacaoTorreGelo5 <- loadBMP "resources/textures/towers/animacoesTorreGelo/5.bmp"
  animacaoTorreGelo6 <- loadBMP "resources/textures/towers/animacoesTorreGelo/6.bmp"
  animacaoTorreGelo7 <- loadBMP "resources/textures/towers/animacoesTorreGelo/7.bmp"
  animacaoTorreGelo8 <- loadBMP "resources/textures/towers/animacoesTorreGelo/8.bmp"
  animacaoTorreGelo9 <- loadBMP "resources/textures/towers/animacoesTorreGelo/9.bmp"
  animacaoTorreGelo10 <- loadBMP "resources/textures/towers/animacoesTorreGelo/10.bmp"
  animacaoTorreGelo11 <- loadBMP "resources/textures/towers/animacoesTorreGelo/11.bmp"
  animacaoTorreGelo12 <- loadBMP "resources/textures/towers/animacoesTorreGelo/12.bmp"
  animacaoTorreGelo13 <- loadBMP "resources/textures/towers/animacoesTorreGelo/13.bmp"
  animacaoTorreGelo14 <- loadBMP "resources/textures/towers/animacoesTorreGelo/14.bmp"
  animacaoTorreGelo15 <- loadBMP "resources/textures/towers/animacoesTorreGelo/15.bmp"
  animacaoTorreGelo16 <- loadBMP "resources/textures/towers/animacoesTorreGelo/16.bmp"
  animacaoTorreGelo17 <- loadBMP "resources/textures/towers/animacoesTorreGelo/17.bmp"
  animacaoTorreGelo18 <- loadBMP "resources/textures/towers/animacoesTorreGelo/18.bmp"
  animacaoTorreGelo19 <- loadBMP "resources/textures/towers/animacoesTorreGelo/19.bmp"
  animacaoTorreGelo20 <- loadBMP "resources/textures/towers/animacoesTorreGelo/20.bmp"
  animacaoTorreGelo21 <- loadBMP "resources/textures/towers/animacoesTorreGelo/21.bmp"
  animacaoTorreGelo22 <- loadBMP "resources/textures/towers/animacoesTorreGelo/22.bmp"
  animacaoTorreGelo23 <- loadBMP "resources/textures/towers/animacoesTorreGelo/23.bmp"
  animacaoTorreGelo24 <- loadBMP "resources/textures/towers/animacoesTorreGelo/24.bmp"
  animacaoTorreGelo25 <- loadBMP "resources/textures/towers/animacoesTorreGelo/25.bmp"
  animacaoTorreGelo26 <- loadBMP "resources/textures/towers/animacoesTorreGelo/26.bmp"
  animacaoTorreGelo27 <- loadBMP "resources/textures/towers/animacoesTorreGelo/27.bmp"
  animacaoTorreGelo28 <- loadBMP "resources/textures/towers/animacoesTorreGelo/28.bmp"
  animacaoTorreGelo29 <- loadBMP "resources/textures/towers/animacoesTorreGelo/29.bmp"
  animacaoTorreFogo1 <- loadBMP "resources/textures/towers/animacoesTorreFogo/1.bmp"
  animacaoTorreFogo2 <- loadBMP "resources/textures/towers/animacoesTorreFogo/2.bmp"
  animacaoTorreFogo3 <- loadBMP "resources/textures/towers/animacoesTorreFogo/3.bmp"
  animacaoTorreFogo4 <- loadBMP "resources/textures/towers/animacoesTorreFogo/4.bmp"
  animacaoTorreFogo5 <- loadBMP "resources/textures/towers/animacoesTorreFogo/5.bmp"
  animacaoTorreFogo6 <- loadBMP "resources/textures/towers/animacoesTorreFogo/6.bmp"
  animacaoTorreFogo7 <- loadBMP "resources/textures/towers/animacoesTorreFogo/7.bmp"
  animacaoTorreFogo8 <- loadBMP "resources/textures/towers/animacoesTorreFogo/8.bmp"
  animacaoTorreFogo9 <- loadBMP "resources/textures/towers/animacoesTorreFogo/9.bmp"
  animacaoTorreFogo10 <- loadBMP "resources/textures/towers/animacoesTorreFogo/10.bmp"
  animacaoTorreFogo11 <- loadBMP "resources/textures/towers/animacoesTorreFogo/11.bmp"
  animacaoTorreFogo12 <- loadBMP "resources/textures/towers/animacoesTorreFogo/12.bmp"
  animacaoTorreFogo13 <- loadBMP "resources/textures/towers/animacoesTorreFogo/13.bmp"
  animacaoTorreFogo14 <- loadBMP "resources/textures/towers/animacoesTorreFogo/14.bmp"
  animacaoTorreFogo15 <- loadBMP "resources/textures/towers/animacoesTorreFogo/15.bmp"
  animacaoTorreFogo16 <- loadBMP "resources/textures/towers/animacoesTorreFogo/16.bmp"
  animacaoTorreFogo17 <- loadBMP "resources/textures/towers/animacoesTorreFogo/17.bmp"
  animacaoTorreFogo18 <- loadBMP "resources/textures/towers/animacoesTorreFogo/18.bmp"
  animacaoTorreFogo19 <- loadBMP "resources/textures/towers/animacoesTorreFogo/19.bmp"
  animacaoTorreFogo20 <- loadBMP "resources/textures/towers/animacoesTorreFogo/20.bmp"
  animacaoTorreFogo21 <- loadBMP "resources/textures/towers/animacoesTorreFogo/21.bmp"
  animacaoTorreFogo22 <- loadBMP "resources/textures/towers/animacoesTorreFogo/22.bmp"
  animacaoTorreFogo23 <- loadBMP "resources/textures/towers/animacoesTorreFogo/23.bmp"
  animacaoTorreFogo24 <- loadBMP "resources/textures/towers/animacoesTorreFogo/24.bmp"
  animacaoTorreFogo25 <- loadBMP "resources/textures/towers/animacoesTorreFogo/25.bmp"
  animacaoTorreFogo26 <- loadBMP "resources/textures/towers/animacoesTorreFogo/26.bmp"
  animacaoTorreFogo27 <- loadBMP "resources/textures/towers/animacoesTorreFogo/27.bmp"
  animacaoTorreFogo28 <- loadBMP "resources/textures/towers/animacoesTorreFogo/28.bmp"
  animacaoTorreFogo29 <- loadBMP "resources/textures/towers/animacoesTorreFogo/29.bmp"
  animacaoTorreResina1 <- loadBMP "resources/textures/towers/animacoesTorreResina/1.bmp"
  animacaoTorreResina2 <- loadBMP "resources/textures/towers/animacoesTorreResina/2.bmp"
  animacaoTorreResina3 <- loadBMP "resources/textures/towers/animacoesTorreResina/3.bmp"
  animacaoTorreResina4 <- loadBMP "resources/textures/towers/animacoesTorreResina/4.bmp"
  animacaoTorreResina5 <- loadBMP "resources/textures/towers/animacoesTorreResina/5.bmp"
  animacaoTorreResina6 <- loadBMP "resources/textures/towers/animacoesTorreResina/6.bmp"
  animacaoTorreResina7 <- loadBMP "resources/textures/towers/animacoesTorreResina/7.bmp"
  animacaoTorreResina8 <- loadBMP "resources/textures/towers/animacoesTorreResina/8.bmp"
  animacaoTorreResina9 <- loadBMP "resources/textures/towers/animacoesTorreResina/9.bmp"
  animacaoTorreResina10 <- loadBMP "resources/textures/towers/animacoesTorreResina/10.bmp"
  animacaoTorreResina11 <- loadBMP "resources/textures/towers/animacoesTorreResina/11.bmp"
  animacaoTorreResina12 <- loadBMP "resources/textures/towers/animacoesTorreResina/12.bmp"
  animacaoTorreResina13 <- loadBMP "resources/textures/towers/animacoesTorreResina/13.bmp"
  animacaoTorreResina14 <- loadBMP "resources/textures/towers/animacoesTorreResina/14.bmp"
  animacaoTorreResina15 <- loadBMP "resources/textures/towers/animacoesTorreResina/15.bmp"
  animacaoTorreResina16 <- loadBMP "resources/textures/towers/animacoesTorreResina/16.bmp"
  animacaoTorreResina17 <- loadBMP "resources/textures/towers/animacoesTorreResina/17.bmp"
  animacaoTorreResina18 <- loadBMP "resources/textures/towers/animacoesTorreResina/18.bmp"
  animacaoTorreResina19 <- loadBMP "resources/textures/towers/animacoesTorreResina/19.bmp"
  animacaoTorreResina20 <- loadBMP "resources/textures/towers/animacoesTorreResina/20.bmp"
  animacaoTorreResina21 <- loadBMP "resources/textures/towers/animacoesTorreResina/21.bmp"
  animacaoTorreResina22 <- loadBMP "resources/textures/towers/animacoesTorreResina/22.bmp"
  animacaoTorreResina23 <- loadBMP "resources/textures/towers/animacoesTorreResina/23.bmp"
  animacaoTorreResina24 <- loadBMP "resources/textures/towers/animacoesTorreResina/24.bmp"
  animacaoTorreResina25 <- loadBMP "resources/textures/towers/animacoesTorreResina/25.bmp"
  animacaoTorreResina26 <- loadBMP "resources/textures/towers/animacoesTorreResina/26.bmp"
  animacaoTorreResina27 <- loadBMP "resources/textures/towers/animacoesTorreResina/27.bmp"
  animacaoTorreResina28 <- loadBMP "resources/textures/towers/animacoesTorreResina/28.bmp"
  animacaoTorreResina29 <- loadBMP "resources/textures/towers/animacoesTorreResina/29.bmp"
  base <- loadBMP "resources/textures/base/Base.bmp"
  portal <- loadBMP "resources/textures/portal/Portal.bmp"
  guerreiroFogo <- loadBMP "resources/textures/entities/GuerreiroFogo.bmp"
  mulherLanca <- loadBMP "resources/textures/entities/MulherLanca.bmp"
  creditos <- loadBMP "resources/textures/ui/novoCreditos.bmp"
  vida <- loadBMP "resources/textures/ui/vida.bmp"
  fundoMenu <- loadBMP "resources/textures/menuFundo/fundoMenu.bmp"
  botaoPlay <- loadBMP "resources/textures/menuFundo/botaoPlay.bmp"
  botaoCredito <- loadBMP "resources/textures/menuFundo/botaoCredits.bmp"
  botaoLevel <- loadBMP "resources/textures/menuFundo/botaoLevel.bmp"
  fundoJogo <- loadBMP "resources/textures/menuFundo/novoFundoJogo.bmp"
  numero0 <- loadBMP "resources/textures/numbers/0.bmp"
  numero1 <- loadBMP "resources/textures/numbers/1.bmp"
  numero2 <- loadBMP "resources/textures/numbers/2.bmp"
  numero3 <- loadBMP "resources/textures/numbers/3.bmp"
  numero4 <- loadBMP "resources/textures/numbers/4.bmp"
  numero5 <- loadBMP "resources/textures/numbers/5.bmp"
  numero6 <- loadBMP "resources/textures/numbers/6.bmp"
  numero7 <- loadBMP "resources/textures/numbers/7.bmp"
  numero8 <- loadBMP "resources/textures/numbers/8.bmp"
  numero9 <- loadBMP "resources/textures/numbers/9.bmp"
  banner1 <- loadBMP "resources/textures/ui/banner1_1.bmp"
  banner2 <- loadBMP "resources/textures/ui/banner1_2.bmp"
  banner3 <- loadBMP "resources/textures/ui/banner1_3.bmp"
  lojaFundo <- loadBMP "resources/textures/ui/fundoLoja2.bmp"
  bannerLoja <- loadBMP "resources/textures/ui/banner_.bmp"
  creditosJogador <- loadBMP "resources/textures/ui/creditosJogador.bmp"
  iconeVidaJg <- loadBMP "resources/textures/ui/iconeVidaJg.bmp"
  iconeLoja <- loadBMP "resources/textures/ui/iconeLoja.bmp"
  fundoTorre <- loadBMP "resources/textures/ui/fundoTorre.bmp"
  molduraMapa <- loadBMP "resources/textures/ui/molduraMapa.bmp"
  sM <- loadBMP "resources/textures/letras/S.bmp"
  t <- loadBMP "resources/textures/letras/t.bmp"
  o <- loadBMP "resources/textures/letras/o.bmp"
  r <- loadBMP "resources/textures/letras/r.bmp"
  e <- loadBMP "resources/textures/letras/e.bmp"
  play janela 
        fundo 
        fr 
        (itInicial 
          [
            ("terra",terra),         --64x64 px
            ("relva",relva),         --64x64 px
            ("agua",agua),          --64x64 px
            ("torreGelo",torreGelo),     --64x121 px
            ("torreResina",torreResina),   --64x121 px
            ("torreFogo",torreFogo),     --64x121 px
            ("base",base),          --64x104 px
            ("portal",portal),         
            ("guerreiroFogo",guerreiroFogo), --27x47 px
            ("mulherLanca",mulherLanca),   --27x50 px
            ("creditos",creditos),       --13x21 px
            ("vida", vida), --18x16 px
            ("fundoMenu",fundoMenu), 
            ("botaoPlay",botaoPlay), 
            ("botaoCredito",botaoCredito), 
            ("botaoLevel",botaoLevel),
            ("fundoJogo", fundoJogo), --1920x1080 px
            ("animacaoTorreGelo1", animacaoTorreGelo1),
            ("animacaoTorreGelo2", animacaoTorreGelo2),
            ("animacaoTorreGelo3", animacaoTorreGelo3),
            ("animacaoTorreGelo4", animacaoTorreGelo4),
            ("animacaoTorreGelo5", animacaoTorreGelo5),
            ("animacaoTorreGelo6", animacaoTorreGelo6),
            ("animacaoTorreGelo7", animacaoTorreGelo7),
            ("animacaoTorreGelo8", animacaoTorreGelo8),
            ("animacaoTorreGelo9", animacaoTorreGelo9),
            ("animacaoTorreGelo10", animacaoTorreGelo10),
            ("animacaoTorreGelo11", animacaoTorreGelo11),
            ("animacaoTorreGelo12", animacaoTorreGelo12),
            ("animacaoTorreGelo13", animacaoTorreGelo13),
            ("animacaoTorreGelo14", animacaoTorreGelo14),
            ("animacaoTorreGelo15", animacaoTorreGelo15),
            ("animacaoTorreGelo16", animacaoTorreGelo16),
            ("animacaoTorreGelo17", animacaoTorreGelo17),
            ("animacaoTorreGelo18", animacaoTorreGelo18),
            ("animacaoTorreGelo19", animacaoTorreGelo19),
            ("animacaoTorreGelo20", animacaoTorreGelo20),
            ("animacaoTorreGelo21", animacaoTorreGelo21),
            ("animacaoTorreGelo22", animacaoTorreGelo22),
            ("animacaoTorreGelo23", animacaoTorreGelo23),
            ("animacaoTorreGelo24", animacaoTorreGelo24),
            ("animacaoTorreGelo25", animacaoTorreGelo25),
            ("animacaoTorreGelo26", animacaoTorreGelo26),
            ("animacaoTorreGelo27", animacaoTorreGelo27),
            ("animacaoTorreGelo28", animacaoTorreGelo28),
            ("animacaoTorreGelo29", animacaoTorreGelo29),
            ("animacaoTorreFogo1", animacaoTorreFogo1),
            ("animacaoTorreFogo2", animacaoTorreFogo2),
            ("animacaoTorreFogo3", animacaoTorreFogo3),
            ("animacaoTorreFogo4", animacaoTorreFogo4),
            ("animacaoTorreFogo5", animacaoTorreFogo5),
            ("animacaoTorreFogo6", animacaoTorreFogo6),
            ("animacaoTorreFogo7", animacaoTorreFogo7),
            ("animacaoTorreFogo8", animacaoTorreFogo8),
            ("animacaoTorreFogo9", animacaoTorreFogo9),
            ("animacaoTorreFogo10", animacaoTorreFogo10),
            ("animacaoTorreFogo11", animacaoTorreFogo11),
            ("animacaoTorreFogo12", animacaoTorreFogo12),
            ("animacaoTorreFogo13", animacaoTorreFogo13),
            ("animacaoTorreFogo14", animacaoTorreFogo14),
            ("animacaoTorreFogo15", animacaoTorreFogo15),
            ("animacaoTorreFogo16", animacaoTorreFogo16),
            ("animacaoTorreFogo17", animacaoTorreFogo17),
            ("animacaoTorreFogo18", animacaoTorreFogo18),
            ("animacaoTorreFogo19", animacaoTorreFogo19),
            ("animacaoTorreFogo20", animacaoTorreFogo20),
            ("animacaoTorreFogo21", animacaoTorreFogo21),
            ("animacaoTorreFogo22", animacaoTorreFogo22),
            ("animacaoTorreFogo23", animacaoTorreFogo23),
            ("animacaoTorreFogo24", animacaoTorreFogo24),
            ("animacaoTorreFogo25", animacaoTorreFogo25),
            ("animacaoTorreFogo26", animacaoTorreFogo26),
            ("animacaoTorreFogo27", animacaoTorreFogo27),
            ("animacaoTorreFogo28", animacaoTorreFogo28),
            ("animacaoTorreFogo29", animacaoTorreFogo29),
            ("animacaoTorreResina1", animacaoTorreResina1),
            ("animacaoTorreResina2", animacaoTorreResina2),
            ("animacaoTorreResina3", animacaoTorreResina3),
            ("animacaoTorreResina4", animacaoTorreResina4),
            ("animacaoTorreResina5", animacaoTorreResina5),
            ("animacaoTorreResina6", animacaoTorreResina6),
            ("animacaoTorreResina7", animacaoTorreResina7),
            ("animacaoTorreResina8", animacaoTorreResina8),
            ("animacaoTorreResina9", animacaoTorreResina9),
            ("animacaoTorreResina10", animacaoTorreResina10),
            ("animacaoTorreResina11", animacaoTorreResina11),
            ("animacaoTorreResina12", animacaoTorreResina12),
            ("animacaoTorreResina13", animacaoTorreResina13),
            ("animacaoTorreResina14", animacaoTorreResina14),
            ("animacaoTorreResina15", animacaoTorreResina15),
            ("animacaoTorreResina16", animacaoTorreResina16),
            ("animacaoTorreResina17", animacaoTorreResina17),
            ("animacaoTorreResina18", animacaoTorreResina18),
            ("animacaoTorreResina19", animacaoTorreResina19),
            ("animacaoTorreResina20", animacaoTorreResina20),
            ("animacaoTorreResina21", animacaoTorreResina21),
            ("animacaoTorreResina22", animacaoTorreResina22),
            ("animacaoTorreResina23", animacaoTorreResina23),
            ("animacaoTorreResina24", animacaoTorreResina24),
            ("animacaoTorreResina25", animacaoTorreResina25),
            ("animacaoTorreResina26", animacaoTorreResina26),
            ("animacaoTorreResina27", animacaoTorreResina27),
            ("animacaoTorreResina28", animacaoTorreResina28),
            ("animacaoTorreResina29", animacaoTorreResina29),
            ("numero0", numero0),
            ("numero1", numero1),
            ("numero2", numero2),
            ("numero3", numero3),
            ("numero4", numero4),
            ("numero5", numero5),
            ("numero6", numero6),
            ("numero7", numero7),
            ("numero8", numero8),
            ("numero9", numero9),
            ("banner1", banner1), 
            ("banner2", banner2),
            ("banner3", banner3),
            ("lojaFundo", lojaFundo),
            ("bannerLoja", bannerLoja),
            ("iconeLoja", iconeLoja),
            ("creditosJogador", creditosJogador),
            ("iconeVidaJg", iconeVidaJg),
            ("fundoTorre", fundoTorre),
            ("molduraMapa", molduraMapa),
            ("S", sM),
            ("t", t), 
            ("o", o),
            ("r", r),
            ("e", e)
          ]
        ) 
        desenha 
        reageEventos 
        reageTempo




it1 :: [Textura] -> ImmutableTowers
it1 texturas = 
    ImmutableTowers {estadoIT = Menu, 
                     jogoIT = Jogo {baseJogo = base,
                                    torresJogo = [torre1,torre2,torre3],
                                    portaisJogo = [portal1, portal2],
                                    mapaJogo = mapa,
                                    inimigosJogo = [],
                                    lojaJogo = loja},
                     texturasIT = texturas, 
                     posicaoTorreComprada = (0,0)}

base :: Base
base = Base {vidaBase = 50,
             posicaoBase = (15,9),
             creditosBase = 1000}

torre1 :: Torre
torre1 = Torre {posicaoTorre = (3, 13), 
                projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1}, 
                danoTorre = 5,
                alcanceTorre = 5,
                rajadaTorre = 3,
                cicloTorre = 180,
                tempoTorre = 180,
                iteracoesDesdeInicioAnimacao = 1}

torre2 :: Torre
torre2 = Torre {posicaoTorre = (3, 3), 
                projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}, 
                danoTorre = 3,
                alcanceTorre = 5,
                rajadaTorre = 3,
                cicloTorre = 180,
                tempoTorre = 180,
                iteracoesDesdeInicioAnimacao = 1}

torre3 :: Torre
torre3 = Torre {posicaoTorre = (14, 7), 
                projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}, 
                danoTorre = 1,
                alcanceTorre = 5,
                rajadaTorre = 3,
                cicloTorre = 180,
                tempoTorre = 180,
                iteracoesDesdeInicioAnimacao = 1}

portal1 :: Portal
portal1 = Portal {posicaoPortal = (0,9),
                  ondasPortal = [Onda {inimigosOnda = [inimigo1, inimigo2],
                  cicloOnda = 2*60,
                  tempoOnda = 0,
                  entradaOnda = 0}]}

portal2 :: Portal
portal2 = Portal {posicaoPortal = (5,0), 
                  ondasPortal = [Onda {inimigosOnda = [inimigo3,inimigo4],
                  cicloOnda = 2*60,
                  tempoOnda = 0,
                  entradaOnda = 0}]}

inimigo1 :: Inimigo
inimigo1 = Inimigo {posicaoInimigo = (0,9), 
                    tipoInimigo = GuerreiroFogo, 
                    projeteisInimigo = [], 
                    vidaInimigo = 1000, 
                    butimInimigo = 4, 
                    direcaoInimigo = Este, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 1, 
                    caminhoInimigo = [],
                    acDirecao = (0,9)}

inimigo2 :: Inimigo
inimigo2 = Inimigo {posicaoInimigo = (0,9), 
                    tipoInimigo = MulherLanca, 
                    projeteisInimigo = [], 
                    vidaInimigo = 6, 
                    butimInimigo = 4, 
                    direcaoInimigo = Este, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 1,
                    caminhoInimigo = [],
                    acDirecao = (0,9)}

inimigo3 :: Inimigo
inimigo3 = Inimigo {posicaoInimigo = (5,0), 
                    tipoInimigo = GuerreiroFogo, 
                    projeteisInimigo = [], 
                    vidaInimigo = 1000, 
                    butimInimigo = 4, 
                    direcaoInimigo = Norte, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 1, 
                    caminhoInimigo = [],
                    acDirecao = (5,0)}

inimigo4 :: Inimigo
inimigo4 = Inimigo {posicaoInimigo = (5,0), 
                    tipoInimigo = MulherLanca, 
                    projeteisInimigo = [], 
                    vidaInimigo = 6, 
                    butimInimigo = 4, 
                    direcaoInimigo = Norte, 
                    ataqueInimigo = 5, 
                    velocidadeInimigo = 1,
                    caminhoInimigo = [],
                    acDirecao = (5,0)}

mapa :: Mapa 
mapa = 
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
       
loja = [(1000, Torre{projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 10}}),
        (1000, Torre{projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}}),
        (1000, Torre{projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}})
        ]