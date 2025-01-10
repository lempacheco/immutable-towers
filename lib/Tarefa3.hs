{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Letícia Maria de Lima Cavalcanti Pacheco <a112062@alunos.uminho.pt>
              Filipa Raquel Ferreira Peixoto <a107009@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa2
import Tarefa1
import Data.List
import Data.Maybe 



atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t j =
    atualizaPortaisEInimigos 
  $ atualizaAnimacaoInimigos 
  $ atualizaTorres 
  $ atualizaAnimacaoTorres 
  $ atualizaInimigosEBase t j
  
{-| É responsável por atualizar o jogo, relativamente as torres. 

    == __ Comportamento: __ == 
A função atualiza os inimigos, sempre que estes sofrem danos, e atualiza as torres do jogo, sempre que estas lançam projéteis.
-}

atualizaTorres :: Jogo -> Jogo 
atualizaTorres j = j{inimigosJogo = inimigosAtualizados, torresJogo = torresAtualizadas}
    where inimigos = inimigosJogo j
          torres = torresJogo j
          (inimigosAtualizados, torresAtualizadas) = disparaTodosProjeteis torres inimigos

{-| A função 'atualizaPortaisEInimigos' atualiza o estado dos portais e dos inimigos no jogo. 
A função atualiza os inimigos com base nos projéteis disparados pelas torres e a cada vez que um inimigo é lançado no jogo. 
E atualiza os portais, lançando os inimigos.
-}

atualizaPortaisEInimigos :: Jogo -> Jogo
atualizaPortaisEInimigos j = j{inimigosJogo = inimigosNovoAtualizados, portaisJogo = portaisAtualizado}
    where inimigos = inimigosJogo j
          torres = torresJogo j
          portais = portaisJogo j
          (inimigosAtualizados, _) = disparaTodosProjeteis torres inimigos
          (portaisAtualizado, inimigosNovoAtualizados) = lancaTodosPortais portais inimigosAtualizados


{-| A função 'lancaTodosPortais' é responsável por processar todos os portais do jogo, lançando os inimigos no jogo. 
-}

lancaTodosPortais :: [Portal] -> [Inimigo] -> ([Portal], [Inimigo])
lancaTodosPortais [] is = ([], is)
lancaTodosPortais (p:ps) is = let (portalAtualizado,inimigosNovos) = lancaInimigo p is
                                  (restoPortaisAtualizados, inimigosNovosAtualizados) = lancaTodosPortais ps inimigosNovos
                              in (portalAtualizado:restoPortaisAtualizados, inimigosNovosAtualizados)

{-| A função 'disparaTodosProjeteis' é responsável por processar todas as torres do jogo, disparando projéteis contra os inimigos. 
-}

disparaTodosProjeteis :: [Torre] -> [Inimigo] -> ([Inimigo], [Torre])
disparaTodosProjeteis [] is = (is, [])
disparaTodosProjeteis (t:ts) is = let (inimigosPosDisparo,torreAtualizada) = disparaProjeteis t is
                                      (inimigosAtualizados, restoTorresAtualizadas) = disparaTodosProjeteis ts inimigosPosDisparo
                                  in (inimigosAtualizados, torreAtualizada:restoTorresAtualizadas)

atualizaAnimacaoTorres :: Jogo -> Jogo
atualizaAnimacaoTorres j = j {torresJogo = auxAtualizaAnimacaoTorres (torresJogo j) (inimigosJogo j)}

auxAtualizaAnimacaoTorres :: [Torre] -> [Inimigo] -> [Torre]
auxAtualizaAnimacaoTorres [] _ = []
auxAtualizaAnimacaoTorres (t:ts) is
    | its == 29 = t {iteracoesDesdeInicioAnimacao = 1} : auxAtualizaAnimacaoTorres ts is
    | its /= 1 = t {iteracoesDesdeInicioAnimacao = its + 1} : auxAtualizaAnimacaoTorres ts is
    | tempoTorre t == 0 && inimigosNoAlcance t is /= [] = t {iteracoesDesdeInicioAnimacao = 2} : auxAtualizaAnimacaoTorres ts is
    | otherwise = t : auxAtualizaAnimacaoTorres ts is
        where its = iteracoesDesdeInicioAnimacao t

atualizaAnimacaoInimigos :: Jogo -> Jogo
atualizaAnimacaoInimigos j =
    let is = inimigosJogo j
    in j {inimigosJogo = auxAtualizaAnimacaoInimigos is}

auxAtualizaAnimacaoInimigos :: [Inimigo] -> [Inimigo]
auxAtualizaAnimacaoInimigos [] = []
auxAtualizaAnimacaoInimigos (i:is)
    | Gelo `elem` getTiposProjsInimigo i = i {iteracoesDesdeInicioAnimacaoInimigo = 0} : auxAtualizaAnimacaoInimigos is
    | its == 32 = i {iteracoesDesdeInicioAnimacaoInimigo = 1} : auxAtualizaAnimacaoInimigos is --reseta animaçao correr
    | otherwise = i {iteracoesDesdeInicioAnimacaoInimigo = its + 1} : auxAtualizaAnimacaoInimigos is
        where its = iteracoesDesdeInicioAnimacaoInimigo i

atualizaInimigosEBase :: Tempo -> Jogo -> Jogo
atualizaInimigosEBase t j =
    let is = inimigosJogo j
        b = baseJogo j
        m = mapaJogo j
        (nB, nIs) = inimigoAtingeBase b is
        (nnB, nnIs) = inimigosSemVida nB nIs
    in j { inimigosJogo = atualizaDistanciaPercorridaInimigos t
                            $ atualizaInimigoFogo
                            $ map atualizaDuracaoProjeteisInimigos 
                            $ map moveInimigo 
                            $ geraCaminhos nnIs m nnB,
                            baseJogo = nnB
                         }
 

{-| A função 'atualizaDuracaoProjeteisInimigos' atualiza a duração dos projéteis que estão afetando o inimigo.

Essa função verifica todos os projéteis associados a um inimigo e:
- Reduz a duração dos projéteis do tipo 'Finita'.
- Remove projéteis cuja duração chegou a zero. 

-}

atualizaDuracaoProjeteisInimigos :: Inimigo -> Inimigo 
atualizaDuracaoProjeteisInimigos i = i {projeteisInimigo = projeteisAtualizados} 
    where projeteis = projeteisInimigo i 
          projeteisAtualizados = duracaoFogoOuGelo projeteis

{-| A função 'duracaoFogoOuGelo' processa uma lista de projéteis, atualizando sua duração e removendo projéteis expirados.

-}

duracaoFogoOuGelo :: [Projetil] -> [Projetil] 
duracaoFogoOuGelo [] = []
duracaoFogoOuGelo (p:ps) = case duracaoProjetil p of 
    Finita n -> if (n-1) <= 0 then duracaoFogoOuGelo ps else p {duracaoProjetil = Finita (n - 1)} : duracaoFogoOuGelo ps
    _ -> p : duracaoFogoOuGelo ps

{-| A função 'atualizaBase' é responsável por atualizar a base do jogo. -}
{- 
atualizaBase :: Jogo -> Jogo
atualizaBase j =  
    let b = baseJogo j
        is = inimigosJogo j
    in inimigoAtingeBase $ j {baseJogo = inimigosSemVidaB is b} 
 -}
{-| A função 'detetarInimigo' deteta os inimigos que estão no alcance de uma determinada torre. 
-}

detetarInimigo :: Torre -> [Inimigo] -> [Inimigo]
detetarInimigo torre inimigos =  inimigosNoAlcance torre inimigos

{-| A função 'disparaProjeteis' simula o disparo de projéteis de uma torre contra os inimigos ao seu alcance, 
    respeitando o ciclo de recarga da torre. 
    
    == __ Comportamento: __
    A cada chamada a função verifica se a torre está pronta para disparar, com base no parâmetro *tempoTorre*, 
    o tempo restante. 

    1. Se a torre não estiver pronta para disparar ('tempoTorre > 0') o tempo restante é decrementado, e os inimigos permanecem inalterados. 
    2. Se a torre estiver pronta para disparar ('tempoTorre == 0'), verifica os inimigos dentro do alcance da torre: 
       2.1. Não há inimigos: a função não realiza disparos. 
       2.2. Há inimigos no alcance: a função realiza disparos, a lista de inimigos é atualizada, i.e. aplica-se 
            os danos e os efeitos dos projéteis nos inimigos. E, após disparar o tempo de recarga da torre é reiniciado para o valor do ciclo ('cicloTorre').
    -}

disparaProjeteis :: Torre -> [Inimigo] -> ([Inimigo], Torre)
disparaProjeteis torre [] = ([], torre)
disparaProjeteis torre is
    | tempoTorre torre > 0 = (is, torre {tempoTorre = tempoTorre torre - 1})
    | null (inimigosNoAlcance torre is) = (is,torre)  -- se não tiver inimigos no alcance vai devolver na mesma inimigos, mas a torre não é alterada. 
    | otherwise = (inimigosSobreviventes torre is, novaTorre)
       where novaTorre = torre {tempoTorre = cicloTorre torre}


{-| A função 'inimigosOrdenados' ordena uma lista de inimigos com base na distância
  de cada inimigo em relação a uma torre. Os inimigos mais próximos da torre aparecem 
  primeiro na lista resultante.
  
-}

inimigosOrdenados :: Torre -> [Inimigo] -> [Inimigo]
inimigosOrdenados torre inimigos = sortOn (distinimigo torre) (inimigos)

{-| A função 'inimigosSobreviventes' filtra os inimigos do mapa, que sãoa sobreviventes, e aplica 
    os danos e os efeitos dos projéteis nestes inimigos, tendo em conta o número máximo de inimigos que 
    podem ser atacados de uma só vez.

-}

inimigosSobreviventes :: Torre -> [Inimigo] -> [Inimigo]
inimigosSobreviventes torre inimigos =
        let nI = tirosPossiveis torre inimigos
            inimigosEmOrdem = inimigosOrdenados torre inimigos
            inimigosAtualizados = map (atingeInimigo torre) (take nI inimigosEmOrdem) -- apenas inimigos que tiveram danos
            inimigosSemDano = drop nI inimigosEmOrdem
        in inimigosAtualizados ++ inimigosSemDano

{-| A função 'distinimigo' é responsável por calcular a distância entre uma torre e um inimigo.

-}

distinimigo :: Torre -> Inimigo -> Float
distinimigo t i = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        where (x1, y1) = posicaoInimigo i
              (x2, y2) = posicaoTorre t

{-| A função 'tiroPossiveis' determina o número máximo de tiros que uma torre pode disparar em um ciclo, 
    levando em consideração o número de rajadas da torre, e o número de inimigos no alcance.
-}

tirosPossiveis :: Torre -> [Inimigo] -> Int
tirosPossiveis torre is =
    if rajadaTorre torre < numeroInimigos
        then rajadaTorre torre
        else numeroInimigos
  where numeroInimigos = length (detetarInimigo torre (inimigosOrdenados torre is))


atualizaInimigoGelo :: Inimigo -> Inimigo
atualizaInimigoGelo i = i {velocidadeInimigo = 0}

atualizaInimigoResina :: Inimigo -> Inimigo
atualizaInimigoResina i =
    let f = fatorVelocidadeInimigoResina
    in i {velocidadeInimigo = velocidadeInimigo i * f}

fatorVelocidadeInimigoResina :: Float
fatorVelocidadeInimigoResina = 0.9 --atualizaInimigoResina reduz a velocidade por 10 porcento

atualizaInimigoFogo :: [Inimigo] -> [Inimigo]
atualizaInimigoFogo [] = []
atualizaInimigoFogo (i:is)
    | Fogo `elem` getTiposProjsInimigo i = i {vidaInimigo = vidaInimigo i - taxaVelocidadeInimigoFogo} : atualizaInimigoFogo is
    | otherwise = i : atualizaInimigoFogo is

taxaVelocidadeInimigoFogo :: Float
taxaVelocidadeInimigoFogo = 5/60 --framerate = 60, logo vai retirar 5 de vida por segundo

{-| A função 'inimigosSemVidaIs' devolve a lista de inimigos sobreviventes, cujo parâmetro 'vidaInimigo' seja maior que 0. 
-}
{- 
inimigosSemVidaIs :: [Inimigo] -> [Inimigo]
inimigosSemVidaIs [] = []
inimigosSemVidaIs (i:is)
    | vidaInimigo i <= 0 = inimigosSemVidaIs is
    | otherwise = i : inimigosSemVidaIs is
 -}
 
{-| A função 'inimigosSemVida' é responsável por atualizar os créditos da base, sempre que um inimigo morre. 
-}

inimigosSemVida :: Base -> [Inimigo] -> (Base, [Inimigo])
inimigosSemVida b [] = (b, []) 
inimigosSemVida b (i:is)
    | vidaInimigo i <= 0 = 
        let bAtualizada = b {creditosBase = creditosBase b + butimInimigo i}
        in inimigosSemVida bAtualizada is
    | otherwise = 
        let (bFinal, inimigosRestantes) = inimigosSemVida b is
        in (bFinal, i : inimigosRestantes)


atualizaDistanciaPercorridaInimigos :: Tempo -> [Inimigo] -> [Inimigo]
atualizaDistanciaPercorridaInimigos _ [] = []
atualizaDistanciaPercorridaInimigos t (i:is)  =
    let v = atualizaVelocidadeInimigo i
        (x,y) = posicaoInimigo i
        d = direcaoInimigo i
    in case d of
        Norte -> i {posicaoInimigo = (x, y + (v*t))} : atualizaDistanciaPercorridaInimigos t is
        Sul -> i {posicaoInimigo = (x, y - (v*t))} : atualizaDistanciaPercorridaInimigos t is
        Oeste -> i {posicaoInimigo = (x - (v*t), y)} : atualizaDistanciaPercorridaInimigos t is
        Este -> i {posicaoInimigo = (x + (v*t), y)} : atualizaDistanciaPercorridaInimigos t is
    where
        atualizaVelocidadeInimigo :: Inimigo -> Float
        atualizaVelocidadeInimigo inimigo =
            let tpsProjsInimigo = getTiposProjsInimigo inimigo
            in if Gelo `elem` tpsProjsInimigo
                then if Resina `elem` tpsProjsInimigo
                    then velocidadeInimigo (atualizaInimigoGelo (atualizaInimigoResina inimigo))
                    else velocidadeInimigo (atualizaInimigoGelo inimigo)
                else if Resina `elem` tpsProjsInimigo
                    then velocidadeInimigo (atualizaInimigoResina inimigo)
                    else velocidadeInimigo inimigo 

{-| A função 'inimigoAtingeBase' é responsável por atualizar a lista de inimigos ativos. 
    Sempre que o inimigo atinja a base, este é retirado do mapa. 
-}

inimigoAtingeBase :: Base -> [Inimigo] -> (Base,[Inimigo])
inimigoAtingeBase base [] = (base,[])
inimigoAtingeBase base (i:is) = 
    let (xI, yI) = posicaoInimigo i
        (xB, yB) = posicaoBase base
    in if (xI >= xB-0.5 && xI <= xB+0.5) && (yI >= yB-0.5 && yI <= yB+0.5)
        then inimigoAtingeBase base {vidaBase = vidaBase base - ataqueInimigo i} is
        else (fst (inimigoAtingeBase base is), i : snd (inimigoAtingeBase base is))

{-| A função 'ondaAtiva' verifica se uma determinada está ativa. i.e. o parâmetro entradaOnda > 0. A função
    devolve True se a onda estiver ativa, indicando então que esta pode lançar inimigos.
-}

ondaAtiva :: Onda -> Bool
ondaAtiva o = entradaOnda o <= 0

{-|A função 'lancaInimigo' é responsável por gerenciar o lançamento de inimigos de um portal. 

  == __Comportamneto: __ 

  1. **Sem Ondas**: 
      Se o portal não possui ondas, a função apenas retorna o portal e a lista de inimigos inalterada.
  2. **Ondas Inativas**:
      Caso a onda ativa no portal ainda não esteja pronta (parâmetro `entradaOnda` > 0), 
      reduz o tempo restante para ativação da onda (`entradaOnda - 1`) e atualiza o portal.
  3. **Onda Ativa com Tempo Restante**:
      Se a onda está ativa, mas o tempo para lançar o próximo inimigo ainda não chegou (`tempoOnda > 0`),
      reduz o tempo restante (`tempoOnda - 1`) e atualiza o portal.
  4. **Onda Ativa Pronta para Lançar**:
      Se o tempo para lançar o próximo inimigo chegou a 0, reinicia o contador (`tempoOnda` = `cicloOnda`),
      chama a função 'ativaInimigo' para mover o próximo inimigo da onda para a lista de inimigos ativos,
      e atualiza o portal. 
-}

lancaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
lancaInimigo p is = case ondasPortal p of
    [] -> (p, is)
    (o:os)
        | not (ondaAtiva o) ->
            let novoPortal = p {ondasPortal = o':os}
                o' = (o {entradaOnda = (entradaOnda o) - 1})
            in (novoPortal, is)
        | tempoOnda o > 0 ->
            let o' = o {tempoOnda = (tempoOnda o) -1}
                novoPortal = p {ondasPortal = o':os}
            in (novoPortal, is)
        | otherwise ->
            let o' = o {tempoOnda = cicloOnda o}
                p' = p {ondasPortal = o':os}
            in ativaInimigo p' is

geraCaminhos :: [Inimigo] -> Mapa -> Base -> [Inimigo]
geraCaminhos [] _ _ = []
geraCaminhos (i:is) m b =
    let posI = posicaoInimigo i
        posB = posicaoBase b
        caminhos = geraUmCaminho m posI posB [] []
        l = fromJust $ lookup True caminhos
    in if caminhoInimigo i == [] then i {caminhoInimigo = l, direcaoInimigo = head l} : geraCaminhos is m b else i : geraCaminhos is m b

moveInimigo :: Inimigo -> Inimigo
moveInimigo i =
    let (xInicial, yInicial) = acDirecao i
        (xAtual, yAtual) = posicaoInimigo i
        caminho = caminhoInimigo i
    in case caminho of 
        [] -> i 
        (d:[]) 
          | sqrt ((xAtual-xInicial)^2 + (yAtual-yInicial)^2) < 1 -> i
          | otherwise -> i {caminhoInimigo = [d], acDirecao = posicaoInimigo i, direcaoInimigo = d}
        (d:rt) 
          | sqrt ((xAtual-xInicial)^2 + (yAtual-yInicial)^2) < 1 -> i
          | otherwise -> i {caminhoInimigo = rt, acDirecao = posicaoInimigo i, direcaoInimigo = head rt}


baseTds :: Base
baseTds = Base {vidaBase = 100,
             creditosBase = 200} 

inimigo1Tds :: Inimigo
inimigo1Tds = Inimigo {tipoInimigo = Guerreiro, 
                        projeteisInimigo = [], 
                        vidaInimigo = 150, 
                        butimInimigo = 100, 
                        ataqueInimigo = 40, 
                        velocidadeInimigo = 0.5, 
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1}

inimigo2Tds :: Inimigo
inimigo2Tds = Inimigo {tipoInimigo = MulherLanca, 
                        projeteisInimigo = [], 
                        vidaInimigo = 50, 
                        butimInimigo = 150,  
                        ataqueInimigo = 20, 
                        velocidadeInimigo = 1,
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1}

geraOndasPortal :: Int -> Int -> Int -> Posicao -> [Onda]
geraOndasPortal 0 _ _ _ = []
geraOndasPortal qOndas n1 n2 posP = 
  let ondas = geraOndaPortal n1 n2 posP : geraOndasPortal (qOndas-1) n1 n2 posP
  in (last ondas) {tempoOnda = 0} : init ondas

geraOndaPortal :: Int -> Int -> Posicao -> Onda
geraOndaPortal n1 n2 posP = 
  let is1 = geraIs1 posP n1
      is2 = geraIs2 posP n2
  in Onda {inimigosOnda = juntaIs1Is2 is1 is2 0, 
            cicloOnda = 5*60,
            tempoOnda = 10*60,
            entradaOnda = 0
            }

geraIs1 :: Posicao -> Int -> [Inimigo]
geraIs1 posP n1
  | n1 == 0 = []
  | otherwise = inimigo1Tds {posicaoInimigo = posP, acDirecao = posP} : geraIs1 posP (n1-1)

geraIs2 :: Posicao -> Int -> [Inimigo]
geraIs2 posP n2
  | n2 == 0 = []
  | otherwise = inimigo2Tds {posicaoInimigo = posP, acDirecao = posP} : geraIs2 posP (n2-1)

juntaIs1Is2 :: [Inimigo] -> [Inimigo] -> Int -> [Inimigo]
juntaIs1Is2 [] is2 _ = is2
juntaIs1Is2 is1 [] _ = is1
juntaIs1Is2 is1 is2 ac
  | mod ac 2 == 0 = head is1 : juntaIs1Is2 (tail is1) is2 (ac+1)
  | otherwise = head is2 : juntaIs1Is2 is1 (tail is2) (ac+1)
  
loja :: Loja
loja = [ (100, Torre{projetilTorre = Projetil {tipoProjetil = Gelo}}),
         (150, Torre{projetilTorre = Projetil {tipoProjetil = Resina}}),
         (200, Torre{projetilTorre = Projetil {tipoProjetil = Fogo}})
        ]

-- Nível 1

jogo1 :: Jogo
jogo1 = Jogo {baseJogo = base1,
              torresJogo = [],
              portaisJogo = [portal1_1, portal2_1],
              mapaJogo = mapa1,
              inimigosJogo = [],
              lojaJogo = loja
            }

base1 :: Base
base1 = baseTds {posicaoBase = (15,9)}

portal1_1 :: Portal
portal1_1 = Portal {posicaoPortal = (0,9),
                  ondasPortal = geraOndasPortal 1 1 0 (0,9)
                  }

portal2_1 :: Portal
portal2_1 = Portal {posicaoPortal = (5,0), 
                  ondasPortal = geraOndasPortal 1 2 3 (5,0)}

mapa1 :: Mapa 
mapa1 = 
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

-- Nível 2

{- it2 :: [Textura] -> ImmutableTowers
it2 texturas = 
    let it = itTds texturas
    in it {jogoIT = jogo2} -}

jogo2 :: Jogo
jogo2 = Jogo {baseJogo = base2,
              torresJogo = [],
              portaisJogo = [portal1_2, portal2_2, portal3_2],
              mapaJogo = mapa2,
              inimigosJogo = [],
              lojaJogo = loja
            } 

mapa2 :: Mapa 
mapa2 = 
  [ [r,r,r,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [t,t,t,r,r,t,r,r,r,r,r,a,a,r,r,r],
    [r,r,t,r,r,t,t,t,t,t,t,t,t,t,r,r],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,t,r,r],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,t,r,r],
    [r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t],
    [r,r,t,r,r,r,r,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,t,r,r,r,a,a,r,r,t],
    [r,r,t,r,r,r,t,r,r,r,a,a,a,r,r,t],
    [r,r,t,r,r,r,t,r,r,a,a,a,r,t,t,t],
    [r,r,t,r,r,r,t,r,r,a,a,r,r,t,r,r],
    [r,r,t,r,r,r,t,t,t,t,t,t,t,t,r,r],
    [t,t,t,r,r,r,t,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,t,r,r,a,a,r,r,r,r,r],
    [r,r,r,r,r,r,t,r,a,a,a,a,r,r,r,r],
    [r,r,r,r,r,r,t,r,a,a,a,a,r,r,r,r]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

base2 :: Base
base2 = baseTds {posicaoBase = (15,7)}

portal1_2 :: Portal
portal1_2 = Portal {posicaoPortal = (0,1),
                  ondasPortal = geraOndasPortal 1 1 1 (0,1)}

portal2_2 :: Portal
portal2_2 = Portal {posicaoPortal = (0,12), 
                  ondasPortal = geraOndasPortal 0 1 2 (0,12)}

portal3_2 :: Portal
portal3_2 = Portal {posicaoPortal = (5,0), 
                  ondasPortal = geraOndasPortal 0 1 1 (5,0)}

mapa3 :: Mapa 
mapa3 = 
  [ [a,a,a,r,r,r,t,r,r,t,r,r,r,r,r,r],
    [a,t,t,t,t,r,t,r,r,t,r,t,t,t,t,r],
    [r,t,a,a,t,r,t,r,r,t,r,t,r,r,t,r],
    [r,t,a,a,t,r,t,r,r,t,r,t,r,r,t,r],
    [r,t,t,t,t,t,t,r,r,t,t,t,t,t,t,r],
    [r,r,r,r,t,a,a,a,r,r,r,t,r,r,r,r],
    [r,r,r,r,t,a,a,a,a,r,r,t,r,r,r,r],
    [r,r,r,r,t,r,a,a,a,a,r,t,r,r,r,r],
    [r,r,r,r,t,r,r,a,a,a,a,t,r,r,r,r],
    [r,r,r,r,t,r,r,r,a,a,a,t,r,r,r,r],
    [r,t,t,t,t,t,t,r,r,t,t,t,t,t,t,r],
    [r,t,r,r,t,r,t,r,r,t,r,t,a,a,t,r],
    [r,t,r,r,t,r,t,t,t,t,r,t,a,a,t,r],
    [r,t,t,t,t,r,r,r,t,r,r,t,t,t,t,a],
    [r,r,r,r,r,r,r,r,t,r,r,r,r,r,a,a],
    [r,r,r,r,r,r,r,r,t,r,r,r,r,a,a,a]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

jogo3 :: Jogo 
jogo3 = Jogo {mapaJogo = mapa3, 
              inimigosJogo = [], 
              portaisJogo = [portal2_3, portal1_3], 
              torresJogo = [], 
              baseJogo = base3,
              lojaJogo = loja}

base3 = baseTds {posicaoBase = (8,15)}

portal1_3 :: Portal
portal1_3 = Portal {posicaoPortal = (6,0),
                  ondasPortal = geraOndasPortal 1 1 1 (6,0)}

portal2_3 :: Portal
portal2_3 = Portal {posicaoPortal = (9,0), 
                  ondasPortal = geraOndasPortal 0 0 0 (9,0)}

mapa4 :: Mapa 
mapa4 = 
  [
    [a,t,a,a,a,a,a,a,a,a,a,a,a,a,a,a],
    [a,t,a,a,a,a,a,t,t,t,t,t,t,t,t,a],
    [a,t,a,a,a,a,a,t,a,a,a,a,a,a,t,a],
    [a,t,t,t,t,t,t,t,a,r,r,r,r,a,t,a],
    [a,t,a,a,a,a,a,t,a,r,r,r,r,a,t,a],
    [a,t,a,r,r,r,a,t,a,a,a,a,a,a,t,a],
    [a,t,a,r,r,r,a,t,a,a,t,t,t,t,t,a],
    [a,t,a,a,a,a,a,t,a,a,t,a,a,a,t,a],
    [a,t,t,t,t,t,t,t,t,t,t,a,r,a,t,t],
    [a,a,a,a,t,a,a,a,a,a,t,a,r,a,t,a],
    [a,a,a,a,t,a,r,r,r,a,t,a,a,a,t,a],
    [a,a,a,a,t,a,r,r,r,a,t,t,t,t,t,a],
    [a,a,a,a,t,a,a,a,a,a,t,a,a,a,t,a],
    [a,a,a,a,t,t,t,t,t,t,t,a,a,a,t,a],
    [a,a,a,a,a,a,a,a,a,a,a,a,a,a,t,t],
    [a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

jogo4 :: Jogo 
jogo4 = Jogo {mapaJogo = mapa4, 
              inimigosJogo = [], 
              portaisJogo = [portal1_4], 
              torresJogo = [], 
              baseJogo = base4,
              lojaJogo = loja}

base4 = baseTds {posicaoBase = (15,14)}

portal1_4 :: Portal
portal1_4 = Portal {posicaoPortal = (1,0),
                  ondasPortal = geraOndasPortal 1 1 1 (1,0)}

mapa5 :: Mapa 
mapa5 = 
  [
    [t,t,t,a,a,a,a,a,a,a,a,t,a,a,a,a],
    [a,a,t,t,t,t,t,t,a,a,a,t,t,t,t,a],
    [a,a,t,a,a,a,a,t,a,a,a,a,a,a,t,a],
    [a,a,t,a,r,r,a,t,a,r,r,r,r,a,t,a],
    [a,a,t,a,r,r,a,t,a,r,r,r,r,a,t,a],
    [a,a,t,a,a,a,a,t,a,a,a,a,a,a,t,a],
    [a,a,t,t,t,t,t,t,t,t,t,t,t,t,t,a],
    [a,a,a,a,a,a,a,a,a,a,t,a,a,a,t,a],
    [a,a,a,a,a,a,a,a,a,a,t,a,r,a,t,a],
    [a,a,t,t,t,t,t,t,t,t,t,t,t,t,t,a],
    [t,t,t,a,a,a,r,r,r,a,a,a,a,a,t,a],
    [a,a,t,a,a,a,r,r,r,a,a,a,a,a,t,a],
    [a,a,t,a,a,a,a,a,a,a,a,a,a,a,t,a],
    [a,a,t,t,t,t,t,t,a,a,a,a,a,a,t,a],
    [a,a,a,a,a,a,a,t,t,t,t,t,a,t,t,a],
    [a,a,a,a,a,a,a,a,a,a,a,t,t,t,a,a]
  ]
  where
       t = Terra
       r = Relva
       a = Agua

jogo5 :: Jogo 
jogo5 = Jogo {mapaJogo = mapa5, 
              inimigosJogo = [], 
              portaisJogo = [portal1_5], 
              torresJogo = [], 
              baseJogo = base5,
              lojaJogo = loja}

base5 = baseTds {posicaoBase = (15,0)}

portal1_5 :: Portal
portal1_5 = Portal {posicaoPortal = (0,0),
                   ondasPortal = geraOndasPortal 1 1 1 (0,0)}