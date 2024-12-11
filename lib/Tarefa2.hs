{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Letícia Maria de Lima Cavalcanti Pacheco <a112062@alunos.uminho.pt>
              Filipa Raquel Ferreira Peixoto <a107009@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

-- Exemplos para teste 

base2 :: Base
base2 = Base { vidaBase = 100, 
               posicaoBase = (0, 0), 
               creditosBase = 500 }

inimigo2 :: Inimigo 
inimigo2 = Inimigo {posicaoInimigo = (1.5,1.5),
    direcaoInimigo = Norte,
    vidaInimigo = 5,
    velocidadeInimigo = 4.5,
    projeteisInimigo = []}


-- Ondas
onda1 :: Onda
onda1 = Onda {inimigosOnda = [inimigo2], 
              cicloOnda = 2,
              tempoOnda = 2,
              entradaOnda = 0}

onda2 :: Onda
onda2 = Onda {inimigosOnda = [], 
              cicloOnda = 2,
              tempoOnda = 2,
              entradaOnda = 0}

portal1 :: Portal 
portal1 = Portal {posicaoPortal = (0.5,0.5), 
                  ondasPortal = [onda1]}
portal2 :: Portal 
portal2 = Portal {posicaoPortal = (1.5,1.5),
                  ondasPortal = [onda2]}

-- Jogo
jogo1 :: Jogo
jogo1 = Jogo { baseJogo = base2,
               portaisJogo = [portal1, portal2],
               torresJogo = [],
               mapaJogo = [],
               inimigosJogo = [inimigo2],
               lojaJogo = []
              }

{-| A função 'inimigosNoAlcance' filtra os inimigos que estão no alcance de uma determinada torre. 

-}

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance t is = 
  let distinimigo i = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        where (x1, y1) = posicaoInimigo i
              (x2, y2) = posicaoTorre t
  in filter (\i -> distinimigo i <= alcanceTorre t) is

danoInimigo :: Torre -> Inimigo -> Inimigo 
danoInimigo t i = i {vidaInimigo =  (vidaInimigo i - danoTorre t)}


getTiposProjsInimigo :: Inimigo -> [TipoProjetil]
getTiposProjsInimigo i = map tipoProjetil (projeteisInimigo i)

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo
    | eFogoGelo torre inimigo = fogoGelo inimigo
    | eFogoResina torre inimigo = fogoResina inimigo
    | eProjetilIgual torre inimigo = projetilIgual torre inimigo
    | otherwise = outrasCombs torre inimigo
        where
            eFogoGelo :: Torre -> Inimigo -> Bool
            eFogoGelo t i = (tipoProjetil (projetilTorre t) == Fogo && Gelo `elem` getTiposProjsInimigo i) || (tipoProjetil (projetilTorre t) == Gelo && Fogo `elem` getTiposProjsInimigo i)

            fogoGelo :: Inimigo -> Inimigo
            fogoGelo i = i {projeteisInimigo = removeProj Gelo $ removeProj Fogo (projeteisInimigo i)}

            removeProj :: TipoProjetil -> [Projetil] -> [Projetil]
            removeProj _ [] = []
            removeProj tp1 (p:ps)
                | tp1 == tipoProjetil p = removeProj tp1 ps
                | otherwise = p : removeProj tp1 ps

            eFogoResina :: Torre -> Inimigo -> Bool
            eFogoResina t i = (tipoProjetil (projetilTorre t) == Fogo && Resina `elem` getTiposProjsInimigo i) || (tipoProjetil (projetilTorre t) == Resina && Fogo `elem` getTiposProjsInimigo i)

            fogoResina :: Inimigo -> Inimigo
            fogoResina i = i {projeteisInimigo = newProjsInimigo}
                where
                    newProjsInimigo = if Resina `elem` getTiposProjsInimigo i then dobraElem Fogo (removeProj Resina (projeteisInimigo i)) else dobraElem Fogo (projeteisInimigo i)

            dobraElem :: TipoProjetil -> [Projetil] -> [Projetil]
            dobraElem _ [] = []
            dobraElem p1 (p:ps)
                |tipoProjetil p == p1 = case duracaoProjetil p of
                    Finita (f :: Float) -> p{duracaoProjetil = Finita (f * 2)} : ps
                    Infinita -> p:ps
                | otherwise = dobraElem p1 ps

            eProjetilIgual :: Torre -> Inimigo -> Bool
            eProjetilIgual t i = tipoProjetil (projetilTorre t) `elem` getTiposProjsInimigo i

            projetilIgual :: Torre -> Inimigo -> Inimigo
            projetilIgual t i =
                let tpProj = tipoProjetil (projetilTorre t)
                in i {projeteisInimigo = dobraElem tpProj (projeteisInimigo i)}

            outrasCombs :: Torre -> Inimigo -> Inimigo
            outrasCombs t i = i {projeteisInimigo = projetilTorre t : projeteisInimigo i}
                
{-| A função 'ativaInimigo' é responsável por mover o próximo inimigo a ser lanaçado pelo portal para lista de inimigos ativos. 

-}

ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigosAtivos = case ondasPortal portal of 
  [] -> (portal, inimigosAtivos) 
  (o:os) -> case inimigosOnda o of 
    [] -> let novoPortal = portal {ondasPortal = os}
          in (novoPortal, inimigosAtivos) 
    (i:is) -> let novaOnda = o {inimigosOnda = is} 
                  novoPortal = portal {ondasPortal = novaOnda: os}
                  novoInimigo = i: inimigosAtivos
              in (novoPortal, novoInimigo)

terminouJogo :: Jogo -> Bool
terminouJogo j = ganhouJogo j || perdeuJogo j 

ganhouJogo :: Jogo -> Bool
ganhouJogo j = null (inimigosJogo j) 
               && (vidaBase $ baseJogo j) > 0 
               && all verificaPortal (portaisJogo j) 

verificaPortal :: Portal -> Bool
verificaPortal p = null (ondasPortal p) || all (null . inimigosOnda) (ondasPortal p)

perdeuJogo :: Jogo -> Bool
perdeuJogo j = vidaBase (baseJogo j) <= 0 