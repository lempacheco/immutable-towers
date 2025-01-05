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
import Data.Maybe (fromJust)


atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t j = atualizaAnimacaoInimigos $ atualizaInimigos t $ atualizaTorres $ atualizaAnimacaoTorres $ atualizaPortaisEInimigos $ atualizaBase j

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
                                  in (inimigosAtualizados, torreAtualizada:restoTorresAtualizadas )

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

atualizaInimigos :: Tempo -> Jogo -> Jogo
atualizaInimigos t j =
    let is = inimigosJogo j
        b = baseJogo j
        m = mapaJogo j
    in j { inimigosJogo = inimigoAtingeBaseIs b 
                            $ atualizaDistanciaPercorridaInimigos t 
                            $ inimigosSemVidaIs
                            $ atualizaInimigoFogo
                            $ map atualizaDuracaoProjeteisInimigos 
                            $ map moveInimigo 
                            $ geraCaminhos is m b
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
    Finita n -> if n <= 0 then duracaoFogoOuGelo ps else p {duracaoProjetil = Finita (n - 1)} : duracaoFogoOuGelo ps
    _ -> p : duracaoFogoOuGelo ps

{-| A função 'atualizaBase' é responsável por atualizar a base do jogo. -}

atualizaBase :: Jogo -> Jogo
atualizaBase j =
    let b = baseJogo j
        is = inimigosJogo j
    in j {baseJogo = inimigosSemVidaB is $ inimigoAtingeBaseB is b} 

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
    | otherwise = (inimigosSobreviventesAlcance torre is, novaTorre)
       where novaTorre = torre {tempoTorre = cicloTorre torre}


{-| A função 'inimigosOrdenados' ordena uma lista de inimigos com base na distância
  de cada inimigo em relação a uma torre. Os inimigos mais próximos da torre aparecem 
  primeiro na lista resultante.
  
  == __ Exemplos de utilização: __

  >>> let torre = Torre {posicaoTorre = (2.0, 3.0)}
  >>> let inimigos = [Inimigo {posicaoInimigo = (1.0, 1.0)}, Inimigo {posicaoInimigo = (3.0, 3.0)}]
  >>> inimigosOrdenados torre ini[] -> error "não existe um caminho válido"  migos
  [Inimigo {posicaoInimigo = (3.0, 3.0)}, Inimigo {posicaoInimigo = (1.0, 1.0)}]
  
-}

inimigosOrdenados :: Torre -> [Inimigo] -> [Inimigo]
inimigosOrdenados torre inimigos = sortOn (distinimigo torre) (inimigos)

{-| A função 'inimigosSobreviventesAlcance' filtra os inimigos que estão no alcance de uma torre, e aplica 
    os danos e os efeitos dos projéteis nestes inimigos, tendo em conta o número máximo de inimigos que 
    podem ser atacados de uma só vez.

-}

inimigosSobreviventesAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosSobreviventesAlcance torre inimigos =
        let nI = tirosPossiveis torre inimigos
            inimigosEmOrdem = inimigosOrdenados torre inimigos
            inimigosAtualizados = map (atingeInimigo torre) (take nI inimigosEmOrdem) -- apenas inimigos que tiveram danos
            inimigosSemDano = drop nI inimigosEmOrdem
        in (filter (\i -> vidaInimigo i > 0) inimigosAtualizados) ++ inimigosSemDano

{-| A função 'distinimigo' é responsável por calcular a distância entre uma torre e um inimigo.

-}

distinimigo :: Torre -> Inimigo -> Float
distinimigo t i = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        where (x1, y1) = posicaoInimigo i
              (x2, y2) = posicaoTorre t

{-| A função 'tiroPossiveis' determina o número máximo de tiros que uma torre pode disparar em um ciclo, 
    levando em consideração o número de rajadas da torre, e o número de inimigos no alcance.

== __ Exemplos de utilização: __

  >>> tirosPossiveis (Torre {rajadaTorre = 3}) [inimigo1, inimigo2]
  2

  >>> tirosPossiveis (Torre {rajadaTorre = 5}) [inimigo1, inimigo2, inimigo3, inimigo4]
  4
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

inimigosSemVidaIs :: [Inimigo] -> [Inimigo]
inimigosSemVidaIs [] = []
inimigosSemVidaIs (i:is)
    | vidaInimigo i <= 0 = inimigosSemVidaIs is
    | otherwise = i : inimigosSemVidaIs is

{-| A função 'inimigosSemVidaB' é responsável por atualizar os créditos da base, sempre que um inimigo morre. 
-}

inimigosSemVidaB :: [Inimigo] -> Base -> Base
inimigosSemVidaB [] b = b
inimigosSemVidaB (i:is) b
    | vidaInimigo i <= 0 = inimigosSemVidaB is b{creditosBase =  creditosBase b + butimInimigo i}
    | otherwise = inimigosSemVidaB is b

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

{-| A função 'inimigoAtingeBaseIs' é responsável por atualizar a lista de inimigos ativos. 
    Sempre que o inimigo atinja a base, este é retirado do mapa. 
-}

inimigoAtingeBaseIs :: Base -> [Inimigo] -> [Inimigo]
inimigoAtingeBaseIs _ [] = []
inimigoAtingeBaseIs base (i:is) = 
    let (xI, yI) = posicaoInimigo i
        (xB, yB) = posicaoBase base
    in if (xI >= xB-0.5 && xI <= xB+0.5) && (yI >= yB-0.5 && yI <= yB+0.5)
        then inimigoAtingeBaseIs base is
        else i : inimigoAtingeBaseIs base is 

{-| A função 'inimigoAtingeBaseB' é responsável por atualizar a vida da base sempre que o inimigo atinja. 
-}

inimigoAtingeBaseB :: [Inimigo] -> Base -> Base
inimigoAtingeBaseB [] base = base
inimigoAtingeBaseB (i:is) base =
    let (xI, yI) = posicaoInimigo i
        (xB, yB) = posicaoBase base
    in if (xI >= xB-0.5 && xI <= xB+0.5) && (yI >= yB-0.5 && yI <= yB+0.5)
        then inimigoAtingeBaseB is (base {vidaBase = vidaBase base - ataqueInimigo i})
        else inimigoAtingeBaseB is base

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

{- verificaCaminho :: Mapa -> Posicao -> Posicao -> [Posicao] -> [Direcao] -> (Bool, [Posicao], [Direcao])
verificaCaminho m pos@(x,y) posB lpos ld = 
    let (resultado_bool, resultado_lpos, resultado_ld) = geraUmCaminho m pos posB lpos ld
    in if resultado_bool == False 
        then case last resultado_ld of
            Norte -> geraUmCaminho m (x,y-1) posB resultado_lpos resultado_ld
            Sul -> geraUmCaminho m (x,y+1) posB resultado_lpos resultado_ld
            Este -> geraUmCaminho m (x-1,y) posB resultado_lpos resultado_ld
            Oeste -> geraUmCaminho m (x+1,y) posB resultado_lpos resultado_ld
        else (True, lpos, ld) -}

geraUmCaminho :: Mapa -> Posicao -> Posicao -> [Posicao] -> [Direcao] -> [(Bool, [Direcao])]
geraUmCaminho m pos@(x,y) posB lpos ld
  | chegouBase pos posB = [(True, ld)]
  | verificaDirecaoTerra m pos lpos Norte = geraUmCaminho m (x,y+1) posB (lpos++[(x,y)]) (ld ++ [Norte]) ++ geraUmCaminho m (x,y) posB (lpos++[(x,y+1)]) ld
  | verificaDirecaoTerra m pos lpos Sul = geraUmCaminho m (x,y-1) posB (lpos++[(x,y)]) (ld ++ [Sul]) ++ geraUmCaminho m (x,y) posB (lpos++[(x,y-1)]) ld
  | verificaDirecaoTerra m pos lpos Este = geraUmCaminho m (x+1,y) posB (lpos++[(x,y)]) (ld ++ [Este]) ++ geraUmCaminho m (x,y) posB (lpos++[(x+1,y)]) ld
  | verificaDirecaoTerra m pos lpos Oeste = geraUmCaminho m (x-1,y) posB (lpos++[(x,y)]) (ld ++ [Oeste]) ++ geraUmCaminho m (x,y) posB (lpos++[(x-1,y)]) ld
  | otherwise = [(False, ld)]

moveInimigo :: Inimigo -> Inimigo
moveInimigo i =
    let (xInicial, yInicial) = acDirecao i
        (xAtual, yAtual) = posicaoInimigo i
    in if sqrt ((xAtual-xInicial)^2 + (yAtual-yInicial)^2) < 1
        then i
        else i {caminhoInimigo = tail $ caminhoInimigo i, acDirecao = posicaoInimigo i, direcaoInimigo = head $ tail $ caminhoInimigo i}


