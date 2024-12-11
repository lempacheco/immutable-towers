{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Letícia Maria de Lima Cavalcanti Pacheco <a112062@alunos.uminho.pt>
              Filipa Raquel Ferreira Peixoto <a107009@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa2(getTiposProjsInimigo)

--TODO: 3.3.2.3
--TODO: fatorVelocidadeInimigoResina e taxaVelocidadeInimigoFogo
--TODO: atualizaJogo

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo = undefined

atualizaInimigoGelo :: Inimigo -> Inimigo
atualizaInimigoGelo i = i {velocidadeInimigo = 0}

atualizaInimigoResina :: Inimigo -> Inimigo
atualizaInimigoResina i = 
    let f = fatorVelocidadeInimigoResina
    in i {velocidadeInimigo = velocidadeInimigo i * f}

atualizaInimigoFogo :: Inimigo -> Inimigo
atualizaInimigoFogo i =
    let t = taxaVelocidadeInimigoFogo
    in i {vidaInimigo = vidaInimigo i - t}

fatorVelocidadeInimigoResina :: Float
fatorVelocidadeInimigoResina = undefined

taxaVelocidadeInimigoFogo :: Float
taxaVelocidadeInimigoFogo = undefined

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