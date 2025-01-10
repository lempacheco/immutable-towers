{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Letícia Maria de Lima Cavalcanti Pacheco <a112062@alunos.uminho.pt>
              Filipa Raquel Ferreira Peixoto <a107009@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}

module Tarefa2 where

import LI12425

{-| Filtra os inimigos que estão no alcance de uma determinada torre. 

-}

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance t is = 
  let distinimigo i = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        where (x1, y1) = posicaoInimigo i
              (x2, y2) = posicaoTorre t
  in filter (\i -> distinimigo i <= alcanceTorre t) is

{-| Atualiza o estado da vida do inimigo, assumindo que este acaba de ter sofrido dano.

-}

danoInimigo :: Torre -> Inimigo -> Inimigo 
danoInimigo t i = i {vidaInimigo =  vidaInimigo i - danoTorre t}

{-| Retorna os tipos de projéteis que estão afetando os inimigos.

-}

getTiposProjsInimigo :: Inimigo -> [TipoProjetil]
getTiposProjsInimigo i = map tipoProjetil (projeteisInimigo i)

{-| Aplica os efeitos de projéteis de uma torre a um inimigo.

==__Comportamento:__ 

Verifica as combinações de projéteis e ajusta os efeitos aplicados ao inimigo:

  1. *Fogo e Gelo*:
       Remove os projéteis do tipo Fogo e Gelo do inimigo.
  2. *Fogo e Resina*:
       Remove o projétil de Resina e dobra a duração do projétil de Fogo no inimigo.
  3. *Projetil Igual*:
       Dobra a duração do projétil se o tipo for igual ao projétil disparado pela torre.
  4. *Outras Combinações*:
       Adiciona o projétil da torre ao inimigo.
-}

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo
    | eFogoGelo torre inimigo = danoInimigo torre (fogoGelo inimigo)
    | eFogoResina torre inimigo = danoInimigo torre (fogoResina inimigo)
    | eProjetilIgual torre inimigo = danoInimigo torre (projetilIgual torre inimigo)
    | otherwise = danoInimigo torre (outrasCombs torre inimigo)
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
                
{-| É responsável por mover o próximo inimigo a ser lanaçado pelo portal para lista de inimigos ativos. 

== __Comportamento: __
1. Caso o portal não possua ondas (lista de ondas vazia), o portal permanece inalterado e a lista de inimigos ativos não é modificada. 
2. Caso a primeira onda do portal não tenha inimigos, essa onda é removida do portal. 
3. caso a primeira onda do portal possua inimigos, o primeiro inimigo da onda é movido para a lista de inimigos ativos,
   e a onda é atualizada, removendo este inimigo.

==__Exemplo de utilização:__

>>> let portal1 = Portal {ondasPortal = []}
>>> ativaInimigo portal1 [i1,i2]
(portal1, [i1,i2])

>>> let portal2 = Portal {ondasPortal = [Onda {inimigosOnda = [i1, i2]]}
>>> ativaInimigo portal2 []
(portal2 {ondasPortal = [Onda{inimigosOnda = [i2]}]}, [i1])

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

{-| É responsável por indicar o fim do jogo, sendo possível duas eventualidaes: ganhar ou perder.

-}

terminouJogo :: Jogo -> Bool
terminouJogo j = ganhouJogo j || perdeuJogo j 

{-| A função 'ganhouJogo' indica se um jogador ganhou o jogo, o jogador ganha o jogo nas seguintes condições: 
     1. ausência de inimigos ativos ou inativos;  
     2. nível de vida da base positivo.
-}

ganhouJogo :: Jogo -> Bool
ganhouJogo j = null (inimigosJogo j) 
               && vidaBase  (baseJogo j) > 0 
               && all verificaPortal (portaisJogo j) 

{-| Verifica se um portal está "inativo". Um portal é considerado inativo se:
    1. Não possui nenhuma onda (ondasPortal é uma lista vazia);
    2. Todas as ondas no portal estão vazias, ou seja, não possuem inimigos (inimigosOnda é uma lista vazia para todas as ondas).

-}

verificaPortal :: Portal -> Bool
verificaPortal p = null (ondasPortal p) || all (null . inimigosOnda) (ondasPortal p)

{-| Indica se um jogador perdeu o jogo, o jogador perde o jogo na seguinte condição: 
     1. nível de vida da base igual ou inferior a zero.
-}

perdeuJogo :: Jogo -> Bool
perdeuJogo j = vidaBase (baseJogo j) <= 0 