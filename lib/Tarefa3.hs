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

import Data.List

--TODO: atualizaJogo

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo = undefined

detetarInimigo :: Torre -> [Inimigo] -> [Inimigo]
detetarInimigo torre inimigos =  inimigosNoAlcance torre inimigos

{-| -}
-- mesmo se não tiver inimigos o ciclo está funcionando?
disparaProjeteis :: Torre -> [Inimigo] -> ([Inimigo], Torre)
disparaProjeteis torre [] = ([], torre)
disparaProjeteis torre is = 
    if length (inimigosSobreviventes torre is) == 0 then ([],torre) 
     else if tempoTorre torre > 0 && length (inimigosSobreviventes torre is) > 0 then (is, torre {tempoTorre = tempoTorre torre - 1})
      else (inimigosSobreviventes torre is, novaTorre)
       where novaTorre = torre {tempoTorre = cicloTorre torre} -- (quando chegar a zero dispara)

{-| A função 'inimigosOrdenados' ordena uma lista de inimigos com base na distância
  de cada inimigo em relação a uma torre. Os inimigos mais próximos da torre aparecem 
  primeiro na lista resultante.
  
  == __ Exemplos de utilização: __

  >>> let torre = Torre {posicaoTorre = (2.0, 3.0)}
  >>> let inimigos = [Inimigo {posicaoInimigo = (1.0, 1.0)}, Inimigo {posicaoInimigo = (3.0, 3.0)}]
  >>> inimigosOrdenados torre inimigos
  [Inimigo {posicaoInimigo = (3.0, 3.0)}, Inimigo {posicaoInimigo = (1.0, 1.0)}]
  
-}
inimigosOrdenados :: Torre -> [Inimigo] -> [Inimigo]
inimigosOrdenados torre inimigos = sortOn (distinimigo torre) (detetarInimigo torre inimigos)


-- atualizar apenas o numero de tiros possiveis 

{-| A função 'inimigosSobreviventes' -}

inimigosSobreviventes :: Torre -> [Inimigo] -> [Inimigo]
inimigosSobreviventes torre inimigos =
        let inimigosAtualizados torre inimigos = map (atingeInimigo torre) (take nI inimigosEmOrdem) -- apenas inimigos que tiveram danos
            nI = tirosPossiveis torre inimigos
            inimigosEmOrdem = inimigosOrdenados torre inimigos
            inimigosSemDano = drop nI inimigosEmOrdem
        in (filter (\i -> vidaInimigo i > 0) (inimigosAtualizados torre inimigos)) ++ inimigosSemDano  

{-| A função 'distinimigo' é responsável por calcular a distância entre uma torre e um inimigo.

-}

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
tirosPossiveis torre is = if rajadaTorre torre < numeroInimigos then rajadaTorre torre else numeroInimigos
  where numeroInimigos = length (inimigosOrdenados torre is)


{-
atualizaInimigoGelo :: Inimigo -> Inimigo
atualizaInimigoGelo i = i {velocidadeInimigo = 0}

atualizaInimigoResina :: Inimigo -> Inimigo
atualizaInimigoResina i = 
    let f = fatorVelocidadeInimigoResina
    in i {velocidadeInimigo = velocidadeInimigo i * f}

fatorVelocidadeInimigoResina :: Float
fatorVelocidadeInimigoResina = 1

atualizaInimigoFogo :: Inimigo -> Inimigo
atualizaInimigoFogo i =
    let t = taxaVelocidadeInimigoFogo
    in i {vidaInimigo = vidaInimigo i - t}

taxaVelocidadeInimigoFogo :: Float
taxaVelocidadeInimigoFogo = 1

removeInimigosSemVida :: Portal -> Portal
removeInimigosSemVida p =
    let inimigosAtivos = inimigosOnda $ head $ ondasPortal p
    in p {ondasPortal = (head (ondasPortal p)){inimigosOnda = atualizaInimigosOndaPortal inimigosAtivos} : tail (ondasPortal p)}
        where
            atualizaInimigosOndaPortal :: [Inimigo] -> [Inimigo]
            atualizaInimigosOndaPortal [] = []
            atualizaInimigosOndaPortal (i:is) = if vidaInimigo i == 0
                                                  then atualizaInimigosOndaPortal is
                                                  else i : atualizaInimigosOndaPortal is

atualizaDistanciaPercorridaInimigo :: Tempo -> Inimigo -> Inimigo
atualizaDistanciaPercorridaInimigo t i =
    let v = atualizaVelocidadeInimigo i
        (x,y) = posicaoInimigo i
        d = direcaoInimigo i
    in case d of
        Norte -> i {posicaoInimigo = (x, y - (v*t))}
        Sul -> i {posicaoInimigo = (x, y + (v*t))}
        Oeste -> i {posicaoInimigo = (x - (v*t), y)}
        Este -> i {posicaoInimigo = (x + (v*t), y)}

atualizaVelocidadeInimigo :: Inimigo -> Float
atualizaVelocidadeInimigo i =
    let tpsProjsInimigo = getTiposProjsInimigo i
    in if Gelo `elem` tpsProjsInimigo
        then if Resina `elem` tpsProjsInimigo
              then velocidadeInimigo (atualizaInimigoGelo (atualizaInimigoResina i))
              else velocidadeInimigo (atualizaInimigoGelo i)
        else if Resina `elem` tpsProjsInimigo
            then velocidadeInimigo (atualizaInimigoResina i)
            else velocidadeInimigo i

inimigoAtingeBase :: Inimigo -> Base -> Portal -> (Portal, Base)
inimigoAtingeBase i b
    | posicaoInimigo i == posicaoBase b = (atualizaPortal, atualizaBase)
    | otherwise = b
        where
            atualizaPortal :: Portal -> Inimigo -> Portal
            atualizaPortal portal inimigo =
                let inimigosAtivos = inimigosOnda $ head $ ondasPortal p
                in p {ondasPortal = (atualizaInimigosAtivos i (head ondasPortal)) : tail ondasPortal}
        
            atualizaInimigosAtivos :: Inimigo -> [Inimigo] -> [Inimigo]
            atualizaInimigosAtivos i iAtivos = delete i iAtivos

            atualizaBase :: Base -> Inimigo -> Base
            atualizaBase b i = b {vidaBase = vidaBase b - danoInimigo i}

-}

{-}
-- devolve True, a onda esta ativa, logo pode lançar inimigos
ondaAtiva :: Onda -> Bool 
ondaAtiva o = entradaOnda o <= 0 


--
lancaInimigos :: Portal -> [Inimigo] -> (Portal, [Inimigo]) 
lancaInimigos p is = if ondaAtiva o && tempoOnda o <= 0 then ativaInimigo p is
                     else if ondaAtiva && tempoOnda o > 0 then (p {ondasPortal = (o {tempoOnda  } :os)} , is) 
    where (o:os) = ondasPortal p 



tempoDaOnda :: Portal -> Portal  
tempoDaOnda p = case ondasPortal p of 
    [] -> p 
    (o:os) -> case tempoOnda o of 
        (> 0) ->

            -} 

            