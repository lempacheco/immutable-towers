
module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425

import Tarefa3

itInicial :: [Textura] -> ImmutableTowers
itInicial ts = 
  let it = itTds ts
  in it {jogoIT = jogo1}

itTds :: [Textura] -> ImmutableTowers
itTds texturas = 
    ImmutableTowers {estadoIT = Menu, 
                     texturasIT = texturas, 
                     posicaoTorreComprada = (0,0), 
                     produtoLoja = (-900, 100),
                     jogoItInicial = jogo1, 
                     listaTerreno = [], 
                     listaPortais = [],
                     escolhendoParametros = (0,0,0),
                     modoDeJogo = Nivel1,
                     botaoNivelPassado = (-600, -250)}

janela :: Display
janela = {-InWindow "Immutable Towers" (fromInteger comprimento, fromInteger altura) (0, 0)-} FullScreen

fundo :: Color
fundo = makeColorI 20 60 85 100

fr :: Int
fr = 60

main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"
  terra <- loadBMP "resources/textures/map/Terra2.bmp"
  agua <- loadBMP "resources/textures/map/Agua2.bmp"
  relva <- loadBMP "resources/textures/map/Relva2.bmp"
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
  perfil <- loadBMP "resources/textures/entities/fireWarrior.bmp"
  base <- loadBMP "resources/textures/base/Base.bmp"
  portal <- loadBMP "resources/textures/portal/Portal.bmp"
  guerreiro0 <- loadBMP "resources/textures/entities/guerreiro0.bmp"
  guerreiro1 <- loadBMP "resources/textures/entities/guerreiro1.bmp"
  guerreiro2 <- loadBMP "resources/textures/entities/guerreiro2.bmp"
  guerreiro3 <- loadBMP "resources/textures/entities/guerreiro3.bmp"
  guerreiro4 <- loadBMP "resources/textures/entities/guerreiro4.bmp"
  guerreiro5 <- loadBMP "resources/textures/entities/guerreiro5.bmp"
  guerreiro6 <- loadBMP "resources/textures/entities/guerreiro6.bmp"
  guerreiro7 <- loadBMP "resources/textures/entities/guerreiro7.bmp"
  guerreiro8 <- loadBMP "resources/textures/entities/guerreiro8.bmp"
  mulherLanca0 <- loadBMP "resources/textures/entities/mulherLanca0.bmp"
  mulherLanca1 <- loadBMP "resources/textures/entities/mulherLanca1.bmp"
  mulherLanca2 <- loadBMP "resources/textures/entities/mulherLanca2.bmp"
  mulherLanca3 <- loadBMP "resources/textures/entities/mulherLanca3.bmp"
  mulherLanca4 <- loadBMP "resources/textures/entities/mulherLanca4.bmp"
  mulherLanca5 <- loadBMP "resources/textures/entities/mulherLanca5.bmp"
  mulherLanca6 <- loadBMP "resources/textures/entities/mulherLanca6.bmp"
  mulherLanca7 <- loadBMP "resources/textures/entities/mulherLanca7.bmp"
  mulherLanca8 <- loadBMP "resources/textures/entities/mulherLanca8.bmp"
  vida <- loadBMP "resources/textures/ui/vida.bmp"
  fundoMenu <- loadBMP "resources/textures/menuFundo/fundoMenu.bmp"
  botaoPlay <- loadBMP "resources/textures/menuFundo/botaoPlay.bmp"
  botaoCredito <- loadBMP "resources/textures/menuFundo/botaoCredits.bmp"
  botaoLevel <- loadBMP "resources/textures/menuFundo/botaoLevel.bmp"
  fundoJogo <- loadBMP "resources/textures/menuFundo/fundoPedra.bmp"
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
  creditosJogador <- loadBMP "resources/textures/ui/creditosJogador.bmp"
  molduraMapa2 <- loadBMP "resources/textures/ui/molduraArbusto.bmp"
  moldBaixo <- loadBMP "resources/textures/ui/molduraBaixo.bmp"
  iconeVidaJg <- loadBMP "resources/textures/ui/iconeVidaJg.bmp"
  iconeLoja <- loadBMP "resources/textures/ui/iconeLoja.bmp"
  molduraMapa <- loadBMP "resources/textures/ui/molduraMapa.bmp"
  store <- loadBMP "resources/textures/letras/Store.bmp"
  player <- loadBMP "resources/textures/letras/Player.bmp"
  botaoPausa <- loadBMP "resources/textures/ui/pausaBotao.bmp"
  frasePausa <- loadBMP "resources/textures/ui/frasePausa.bmp"
  fraseContinuar <- loadBMP "resources/textures/ui/fraseContinuar.bmp"
  iconeHome <- loadBMP "resources/textures/ui/botaoHome.bmp"
  iconePausa <- loadBMP "resources/textures/ui/iconePausa.bmp"
  seta <- loadBMP "resources/textures/ui/setaLoja.bmp"
  nInimigos <- loadBMP "resources/textures/ui/nInimigos.bmp"
  botaoQ <- loadBMP "resources/textures/ui/botaoQ.bmp"
  setaCima <- loadBMP "resources/textures/ui/setaCima.bmp"
  setaBaixo <- loadBMP "resources/textures/ui/setaBaixo.bmp"
  fraseLevelWon <- loadBMP "resources/textures/ui/fraseLevelWon.bmp"
  fraseBackToMenuNivelPassado <- loadBMP "resources/textures/ui/fraseBackToMenuNivelPassado.bmp"
  fraseNextLevel <- loadBMP "resources/textures/ui/fraseNextLevel.bmp"
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
            ("guerreiro0",guerreiro0),
            ("guerreiro1",guerreiro1),
            ("guerreiro2",guerreiro2),
            ("guerreiro3",guerreiro3),
            ("guerreiro4",guerreiro4),
            ("guerreiro5",guerreiro5),
            ("guerreiro6",guerreiro6),
            ("guerreiro7",guerreiro7),
            ("guerreiro8",guerreiro8),
            ("mulherLanca0",mulherLanca0),
            ("mulherLanca1",mulherLanca1),
            ("mulherLanca2",mulherLanca2),
            ("mulherLanca3",mulherLanca3),
            ("mulherLanca4",mulherLanca4),
            ("mulherLanca5",mulherLanca5),
            ("mulherLanca6",mulherLanca6),
            ("mulherLanca7",mulherLanca7),
            ("mulherLanca8",mulherLanca8),
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
            ("iconeLoja", iconeLoja),
            ("creditosJogador", creditosJogador),
            ("iconeVidaJg", iconeVidaJg),
            ("molduraMapa", molduraMapa),
            ("molduraMapa2", molduraMapa2),
            ("Store", store),
            ("player", player),
            ("perfil", perfil),
            ("botaoPausa", botaoPausa),
            ("fraseContinuar", fraseContinuar),
            ("frasePausa", frasePausa),
            ("iconeHome", iconeHome), 
            ("iconePausa", iconePausa),
            ("seta", seta),
            ("nInimigos", nInimigos), 
            ("botaoQ", botaoQ),
            ("setaCima", setaCima), 
            ("setaBaixo", setaBaixo), 
            ("moldBaixo", moldBaixo),
            ("fraseLevelWon", fraseLevelWon),
            ("fraseBackToMenuNivelPassado", fraseBackToMenuNivelPassado),
            ("fraseNextLevel", fraseNextLevel)
          ]
        ) 
        desenha 
        reageEventos 
        reageTempo