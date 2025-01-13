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
import Data.Ord

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

{-| Atualiza o estado dos portais e dos inimigos no jogo. 
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


{-| Atualiza o estado das animações das torres no jogo. 

-}


atualizaAnimacaoTorres :: Jogo -> Jogo
atualizaAnimacaoTorres j = j {torresJogo = auxAtualizaAnimacaoTorres (torresJogo j) (inimigosJogo j)}

{-| Processa as animações de cada torre. 

-}

auxAtualizaAnimacaoTorres :: [Torre] -> [Inimigo] -> [Torre]
auxAtualizaAnimacaoTorres [] _ = []
auxAtualizaAnimacaoTorres (t:ts) is
    | its == 29 = t {iteracoesDesdeInicioAnimacao = 1} : auxAtualizaAnimacaoTorres ts is
    | its /= 1 = t {iteracoesDesdeInicioAnimacao = its + 1} : auxAtualizaAnimacaoTorres ts is
    | tempoTorre t == 0 && inimigosNoAlcance t is /= [] = t {iteracoesDesdeInicioAnimacao = 2} : auxAtualizaAnimacaoTorres ts is
    | otherwise = t : auxAtualizaAnimacaoTorres ts is
        where its = iteracoesDesdeInicioAnimacao t

{-| Atualiza o estado das animações dos inimigos no jogo.

-}

atualizaAnimacaoInimigos :: Jogo -> Jogo
atualizaAnimacaoInimigos j =
    let is = inimigosJogo j
    in j {inimigosJogo = auxAtualizaAnimacaoInimigos is}

{-| Processa as animações de cada inimigo. 

-}

auxAtualizaAnimacaoInimigos :: [Inimigo] -> [Inimigo]
auxAtualizaAnimacaoInimigos [] = []
auxAtualizaAnimacaoInimigos (i:is)
    | Gelo `elem` getTiposProjsInimigo i = i {iteracoesDesdeInicioAnimacaoInimigo = 0} : auxAtualizaAnimacaoInimigos is
    | its == 32 = i {iteracoesDesdeInicioAnimacaoInimigo = 1} : auxAtualizaAnimacaoInimigos is --reseta animaçao correr
    | otherwise = i {iteracoesDesdeInicioAnimacaoInimigo = its + 1} : auxAtualizaAnimacaoInimigos is
        where its = iteracoesDesdeInicioAnimacaoInimigo i

{-| Atualiza o estado dos inimigos e da base no jogo.

==__ Comportamento:__ 

1. Movimenta os inimigos sobre a terra, e aplica os efeitos dos projéteis.
2. Atualiza a base, sempre que os inimigos a atingiram, diminuito a vida, e sempre que os inimigos morrem, 
aumentando os créditos.

-}

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
                            $ geraCaminhos nnIs m nnB (acGeraCaminhos j),
                            baseJogo = nnB,
                            acGeraCaminhos = acGeraCaminhos j + 1
                         }
 

{-| Atualiza a duração dos projéteis que estão afetando o inimigo.

Essa função verifica todos os projéteis associados a um inimigo e:
- Reduz a duração dos projéteis do tipo 'Finita'.
- Remove projéteis cuja duração chegou a zero. 

-}

atualizaDuracaoProjeteisInimigos :: Inimigo -> Inimigo 
atualizaDuracaoProjeteisInimigos i = i {projeteisInimigo = projeteisAtualizados} 
    where projeteis = projeteisInimigo i 
          projeteisAtualizados = duracaoFogoOuGelo projeteis

{-| Processa uma lista de projéteis, atualizando sua duração e removendo projéteis expirados.

-}

duracaoFogoOuGelo :: [Projetil] -> [Projetil] 
duracaoFogoOuGelo [] = []
duracaoFogoOuGelo (p:ps) = case duracaoProjetil p of 
    Finita n -> if (n-1) <= 0 then duracaoFogoOuGelo ps else p {duracaoProjetil = Finita (n - 1)} : duracaoFogoOuGelo ps
    _ -> p : duracaoFogoOuGelo ps


{-| Deteta os inimigos que estão no alcance de uma determinada torre. 
-}

detetarInimigo :: Torre -> [Inimigo] -> [Inimigo]
detetarInimigo torre inimigos =  inimigosNoAlcance torre inimigos

{-| Simula o disparo de projéteis de uma torre contra os inimigos ao seu alcance, 
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

{-| É responsável por processar todas as torres do jogo, disparando projéteis contra os inimigos. 
-}

disparaTodosProjeteis :: [Torre] -> [Inimigo] -> ([Inimigo], [Torre])
disparaTodosProjeteis [] is = (is, [])
disparaTodosProjeteis (t:ts) is = let (inimigosPosDisparo,torreAtualizada) = disparaProjeteis t is
                                      (inimigosAtualizados, restoTorresAtualizadas) = disparaTodosProjeteis ts inimigosPosDisparo
                                  in (inimigosAtualizados, torreAtualizada:restoTorresAtualizadas)

{-| Ordena uma lista de inimigos com base na distância
  de cada inimigo em relação a uma torre. Os inimigos mais próximos da torre aparecem 
  primeiro na lista resultante.
  
-}

inimigosOrdenados :: Torre -> [Inimigo] -> [Inimigo]
inimigosOrdenados torre inimigos = sortOn (distinimigo torre) (inimigos)

{-| Filtra os inimigos do mapa, que sãoa sobreviventes, e aplica 
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

{-| É responsável por calcular a distância entre uma torre e um inimigo.

-}

distinimigo :: Torre -> Inimigo -> Float
distinimigo t i = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        where (x1, y1) = posicaoInimigo i
              (x2, y2) = posicaoTorre t

{-| Determina o número máximo de tiros que uma torre pode disparar em um ciclo, 
    levando em consideração o número de rajadas da torre, e o número de inimigos no alcance.
-}

tirosPossiveis :: Torre -> [Inimigo] -> Int
tirosPossiveis torre is =
    if rajadaTorre torre < numeroInimigos
        then rajadaTorre torre
        else numeroInimigos
  where numeroInimigos = length (detetarInimigo torre (inimigosOrdenados torre is))


{-| Atualiza o estado de um inimigo sob o efeito de um projétil de Gelo.

-}

atualizaInimigoGelo :: Inimigo -> Inimigo
atualizaInimigoGelo i = i {velocidadeInimigo = 0}

{-| Atualiza a velocidade de um inimigo sob o efeito de um projétil de Resina. 

-}

atualizaInimigoResina :: Inimigo -> Inimigo
atualizaInimigoResina i =
    let f = fatorVelocidadeInimigoResina
    in i {velocidadeInimigo = velocidadeInimigo i * f}

{-| Fator de redução aplicado ao inimigo sob o efeito de Resina.

==__Comportamento:__
* 0.9 - Representa uma redução de 10% na velocidade do inimigo.

-}

fatorVelocidadeInimigoResina :: Float
fatorVelocidadeInimigoResina = 0.9 --atualizaInimigoResina reduz a velocidade por 10 porcento

{-| Atualiza a vida dos inimigos sob o efeito de projéteis de Fogo.

-}

atualizaInimigoFogo :: [Inimigo] -> [Inimigo]
atualizaInimigoFogo [] = []
atualizaInimigoFogo (i:is)
    | Fogo `elem` getTiposProjsInimigo i = i {vidaInimigo = vidaInimigo i - taxaDanoInimigoFogo} : atualizaInimigoFogo is
    | otherwise = i : atualizaInimigoFogo is

{-| Taxa de dano aplicada a inimigos sob o efeito de projéteis de Fogo.

==__Nota:__ 
5/60 - Reduz 5 pontos de vida por segundo, considerando um framerate de 60 quadros por segundo.
-}

taxaDanoInimigoFogo :: Float
taxaDanoInimigoFogo = 5/60 
 
{-| É responsável por atualizar os créditos da base, sempre que um inimigo morre. 
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


{-| Atualiza a distância percorrida pelos inimigos no jogo, permitindo com que esses se movam.

== __Comportamento:__
Calcula a nova posição do inimigo com base em sua direção, velocidade e o tempo decorrido.

-}

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

{-| É responsável por atualizar a lista de inimigos ativos. 
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

{-| Verifica se uma determinada está ativa. i.e. o parâmetro entradaOnda > 0. A função
    devolve True se a onda estiver ativa, indicando então que esta pode lançar inimigos.
-}

ondaAtiva :: Onda -> Bool
ondaAtiva o = entradaOnda o <= 0

{-| É responsável por gerenciar o lançamento de inimigos de um portal. 

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

{-| É responsável por processar todos os portais do jogo, lançando os inimigos no jogo. 

-}

lancaTodosPortais :: [Portal] -> [Inimigo] -> ([Portal], [Inimigo])
lancaTodosPortais [] is = ([], is)
lancaTodosPortais (p:ps) is = let (portalAtualizado,inimigosNovos) = lancaInimigo p is
                                  (restoPortaisAtualizados, inimigosNovosAtualizados) = lancaTodosPortais ps inimigosNovos
                              in (portalAtualizado:restoPortaisAtualizados, inimigosNovosAtualizados)

{-| Responsável por calcular os caminhos para os inimigos no mapa, em direção a base. 

==__Comportamento:__ 

Para cada inimigo:
1. Verifica a posição atual do inimigo e a posição da base.
2. Gera um caminho usando a função 'geraUmCaminho', a partir da posição do inimigo até a base.
3. Atualiza o inimigo com o caminho gerado e define a próxima direção.


-}

escolheCaminho :: [(Bool, [Direcao])] -> Int -> [Direcao]
escolheCaminho caminhos ac
    | ac >= len = escolheCaminho caminhos (ac-len)
    | otherwise = snd (caminhos !! ac)
    where len = length caminhos

geraCaminhos :: [Inimigo] -> Mapa -> Base -> Int -> [Inimigo]
geraCaminhos [] _ _ _ = []
geraCaminhos (i:is) m b ac =
    let posI = posicaoInimigo i
        posB = posicaoBase b
        caminhos = filter (\v -> fst v == True) (geraUmCaminho m posI posB [] [])
        l = escolheCaminho caminhos ac
    in if caminhoInimigo i == [] then i {caminhoInimigo = l, direcaoInimigo = head l} : geraCaminhos is m b (ac+1) else i : geraCaminhos is m b (ac+1)

{-| Avalia quando é necessário passa para a próxima direção no caminho do inimigo já definido, mudando assim, a direção do inimigo necessário.

-}

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
        (_:rt) 
          | sqrt ((xAtual-xInicial)^2 + (yAtual-yInicial)^2) < 1 -> i
          | otherwise -> i {caminhoInimigo = rt, acDirecao = posicaoInimigo i, direcaoInimigo = head rt}


{-| É responsável por atulizar o jogo, após ter sido realizada, a compra de uma torre. Adiciona a torre nova ao jogo, desde que 
o jogador tenha créditos suficientes, e a torre seja válida, de acordo com as definições da função 'validaTorre'. 

-}

compraTorre :: Torre -> Creditos -> Jogo -> Jogo
compraTorre t custoTorre j 
    | custoTorre <= creditosBase (baseJogo j) && validaTorre j {torresJogo = t: torresJogo j} = jogoNovo
    | otherwise = j 
  where jogoNovo = j {baseJogo = (baseJogo j) {creditosBase = creditosBase (baseJogo j) - custoTorre}, 
                      torresJogo = ordenaTorre  $ t: torresJogo j}

{-| Ordena a lista de torres, com base na coordenada Y da posição de cada torre. 

-}

ordenaTorre :: [Torre] -> [Torre]
ordenaTorre = sortBy (comparing (snd . posicaoTorre)) 


-- Bases do jogo

baseTds :: Base
baseTds = Base {vidaBase = 100,
                creditosBase = 200} 

-- inimigos Homem
inimigo1Tds :: Inimigo
inimigo1Tds = Inimigo {tipoInimigo = Homem, 
                        projeteisInimigo = [], 
                        vidaInimigo = 150, 
                        butimInimigo = 50, 
                        ataqueInimigo = 40, 
                        velocidadeInimigo = 0.5, 
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1}

-- inimigos Mulher
inimigo2Tds :: Inimigo
inimigo2Tds = Inimigo {tipoInimigo = Mulher, 
                        projeteisInimigo = [], 
                        vidaInimigo = 100, 
                        butimInimigo = 45,  
                        ataqueInimigo = 20, 
                        velocidadeInimigo = 1,
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1}


-- loja do jogo. 
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
              lojaJogo = loja,
              acGeraCaminhos = 0
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
    [r,r,r,r,r,r,t,t,t,t,t,t,t,t,r,r],
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

jogo2 :: Jogo
jogo2 = Jogo {baseJogo = base2,
              torresJogo = [],
              portaisJogo = [portal1_2, portal2_2, portal3_2],
              mapaJogo = mapa2,
              inimigosJogo = [],
              lojaJogo = loja,
              acGeraCaminhos = 0
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
                  ondasPortal = geraOndasPortal 1 2 4 (0,1)}

portal2_2 :: Portal
portal2_2 = Portal {posicaoPortal = (0,12), 
                  ondasPortal = geraOndasPortal 2 3 3 (0,12)}

portal3_2 :: Portal
portal3_2 = Portal {posicaoPortal = (5,0), 
                  ondasPortal = geraOndasPortal 2 3 2 (5,0)}

-- Nível 3

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
              lojaJogo = loja,
              acGeraCaminhos = 0}

base3 = baseTds {posicaoBase = (8,15)}

portal1_3 :: Portal
portal1_3 = Portal {posicaoPortal = (6,0),
                  ondasPortal = geraOndasPortal 2 5 3 (6,0)}

portal2_3 :: Portal
portal2_3 = Portal {posicaoPortal = (9,0), 
                  ondasPortal = geraOndasPortal 2 3 2 (9,0)}

-- Nivel 4

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
    [a,t,t,t,t,t,t,t,t,t,t,a,r,a,t,a],
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
              lojaJogo = loja,
              acGeraCaminhos = 0}

base4 :: Base
base4 = baseTds {posicaoBase = (15,14)}

portal1_4 :: Portal
portal1_4 = Portal {posicaoPortal = (1,0),
                  ondasPortal = geraOndasPortal 6 3 2 (1,0)}

-- Nivel 5

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
              portaisJogo = [portal1_5, portal2_5, portal3_5], 
              torresJogo = [], 
              baseJogo = base5,
              lojaJogo = loja,
              acGeraCaminhos = 0}

base5 = baseTds {posicaoBase = (12,15)}

portal1_5 :: Portal
portal1_5 = Portal {posicaoPortal = (0,0),
                   ondasPortal = geraOndasPortal 2 3 1 (0,0)}

portal2_5 :: Portal
portal2_5 = Portal {posicaoPortal = (0,10),
                   ondasPortal = geraOndasPortal 3 2 1 (0,10)}

portal3_5 :: Portal
portal3_5 = Portal {posicaoPortal = (11,0),
                   ondasPortal = geraOndasPortal 3 3 2 (11,0)}

-- Jogo tutorial
jogoTT :: Jogo
jogoTT = Jogo {mapaJogo = mapa1,
                     inimigosJogo = [], 
                     portaisJogo = [portal6_1], 
                     torresJogo = [], 
                     baseJogo = base1, 
                     lojaJogo = loja,
                     acGeraCaminhos = 0}

portal6_1 :: Portal
portal6_1 = Portal {posicaoPortal = (5,0), 
                    ondasPortal = geraOndasPortal 1 3 2 (5,0)}




geraOndasPortal :: Int -> Int -> Int -> Posicao -> [Onda]
geraOndasPortal 0 _ _ _ = []
geraOndasPortal qOndas n1 n2 posP = 
  let ondas = geraOndaPortal n1 n2 posP : geraOndasPortal (qOndas-1) n1 n2 posP
  in (last ondas) {tempoOnda = 0} : init ondas

{-| Cria uma onda de inimigos com base nos parâmetros fornecidos

-}

geraOndaPortal :: Int -- ^ quantidade de inimigos masculinos. 
               -> Int -- ^ quantidade de inimigos femininos. 
               -> Posicao -- ^ posição inicial dos inimigos. 
               -> Onda -- ^ Onda com a configuração definida.
geraOndaPortal n1 n2 posP = 
  let is1 = geraIs1 posP n1
      is2 = geraIs2 posP n2
  in Onda {inimigosOnda = juntaIs1Is2 is1 is2 0, 
            cicloOnda = 5*60,
            tempoOnda = 10*60,
            entradaOnda = 0
            }

{-| Cria um grupo com n inimigos masculinos. 

-}

geraIs1 :: Posicao -> Int -> [Inimigo]
geraIs1 posP n1
  | n1 == 0 = []
  | otherwise = inimigo1Tds {posicaoInimigo = posP, acDirecao = posP} : geraIs1 posP (n1-1)

{-| Cria um grupo com n inimigos femininos. 

-}

geraIs2 :: Posicao -> Int -> [Inimigo]
geraIs2 posP n2
  | n2 == 0 = []
  | otherwise = inimigo2Tds {posicaoInimigo = posP, acDirecao = posP} : geraIs2 posP (n2-1)

{-| Cria um grupo de inimigos, intercalando os masculinos com os femininos. 

-}

juntaIs1Is2 :: [Inimigo] -> [Inimigo] -> Int -> [Inimigo]
juntaIs1Is2 [] is2 _ = is2
juntaIs1Is2 is1 [] _ = is1
juntaIs1Is2 is1 is2 ac
  | mod ac 2 == 0 = head is1 : juntaIs1Is2 (tail is1) is2 (ac+1)
  | otherwise = head is2 : juntaIs1Is2 is1 (tail is2) (ac+1)

