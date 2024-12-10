{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Letícia Maria de Lima Cavalcanti Pacheco <a112062@alunos.uminho.pt>
              Filipa Raquel Ferreira Peixoto <a107009@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance = undefined

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

            getTiposProjsInimigo :: Inimigo -> [TipoProjetil]
            getTiposProjsInimigo i = map tipoProjetil (projeteisInimigo i)

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
                


-- ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
-- ativaInimigo p is = 
--     let i = head (inimigosOnda (head (ondasPortal p)))
--         newOndasP = Onda {inimigosOnda = (tail (inimigosOnda (head (ondasPortal p))) :: Onda) : tail (ondasPortal p)}
--     in (p {ondasPortal = newOndasP}, is)

terminouJogo :: Jogo -> Bool
terminouJogo = undefined

ganhouJogo :: Jogo -> Bool
ganhouJogo = undefined

perdeuJogo :: Jogo -> Bool
perdeuJogo = undefined