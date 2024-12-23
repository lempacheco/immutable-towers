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
--TODO: 3.3.2.3
--TODO: fatorVelocidadeInimigoResina e taxaVelocidadeInimigoFogo
--TODO: atualizaJogo

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo = undefined

detetarInimigo :: Torre -> [Inimigo] -> [Inimigo]
detetarInimigo torre inimigos =  inimigosNoAlcance torre inimigos

{-| ([Inimigo], [Projetil]) -> A lista de inimigos atualizada, refletindo os danos causados pelos projéteis disparados.
A lista de projéteis disparados.-}

disparaProjeteis :: Torre -> [Inimigo] -> ([Inimigo], [Projetil])
disparaProjeteis torre inimigos = 


inimigosOrdenados :: Torre -> [Inimigo] -> [Inimigo]
inimigosOrdenados torre inimigos = sortOn (distinimigo torre) (detetarInimigo torre inimigos)

inimigosAtualizados :: Torre -> [Inimigo] -> [Inimigo]
inimigosAtualizados torre inimigos = map (atingeInimigo torre) (inimigosOrdenados torre inimigos)

inimigosSobreviventes :: Torre -> [Inimigo] -> [Inimigo]
inimigosSobreviventes torre inimigos = 
    filter (\i -> vidaInimigo i > 0) (inimigosAtualizados torre inimigos) 

inimigosOrdenados :: Torre -> [Inimigo] -> [Inimigo]
inimigosOrdenados torre inimigos = sortOn (distinimigo torre) (detetarInimigo torre inimigos)

distinimigo t i = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        where (x1, y1) = posicaoInimigo i
              (x2, y2) = posicaoTorre t

{-| Determina o número de tiros que podem ser disparados neste ciclo -}

tirosPossiveis :: Torre -> [Inimigos] -> Int 
tirosPossiveis torre is = if rajadaTorre torre > numeroInimigos then rajadaTorre torre else numeroInimigos
  where numeroInimigos = length (inimigosOrdenados is)

tempoDisparo :: Torre -> Bool 
tempoDisparo 

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
    | posicaoInimigo i == (atualizaPortal, atualizaBase)
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