{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Letícia Maria de Lima Cavalcanti Pacheco <a112062@alunos.uminho.pt>
              Filipa Raquel Ferreira Peixoto <a107009@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}

module Tarefa1 where
import LI12425

-- | A função 'validaJogo' verifica se um jogo é válido.
validaJogo :: Jogo -> Bool
validaJogo j = validaTorre j && validaBase j && validaPortal j && validaInimigo j 

validaTorre :: Jogo -> Bool 
validaTorre j =
  let ts = torresJogo j
      m = mapaJogo j
  in validaPosicoesTorres ts m 
      && alcanceTorresPositivo ts 
      && rajadaTorresPositivo ts
      && cicloTorresNaoNegativo ts
      && naoSobrepostoTorres ts
      
validaBase :: Jogo -> Bool
validaBase j =
  let b = baseJogo j
      m = mapaJogo j
      ts = torresJogo j
      ps = portaisJogo j
  in validaPosicaoBase b m
      && creditoNaoNegativoBase b
      && naoSobrepostoBaseTorrePortal b ts ps

validaPortal :: Jogo -> Bool 
validaPortal jogo = (peloMenosUmPortal portais) && (validaPosicaoPortal portais mapa) &&
                    (existeCaminho mapa portais base) && (naoSobrepostoBasePortal base portais) && 
                    (naoSobrepostoTorrePortal torres portais)
  where portais@(p:ps) = portaisJogo jogo
        existeCaminho mapa portais base = (existePeloMenosUmCaminho mapa p base) || (existeCaminho mapa portais base)
        torres@(t:ts) = torresJogo jogo
        base = baseJogo jogo
        mapa = mapaJogo jogo
        
validaInimigo :: Jogo -> Bool
validaInimigo jogo = (inimigosInicio inimigos portais) && (inimigosTerra inimigos mapa) &&
                     (inimigosTorre inimigos torres) && (velocidadeInimigos inimigos) && (normalizaInimigos inimigos) 
  where inimigos@(i:is) = inimigosJogo jogo
        portais@(p:ps) = portaisJogo jogo
        torres@(t:ts) = torresJogo jogo
        mapa = mapaJogo jogo

{-| A função 'eTerra' verifica se uma determinada posição é terra. 

-}

eTerra :: Posicao -> Mapa -> Bool
eTerra (x,y) mapa = case procuraTerreno (x,y) mapa of 
  Just Terra -> True 
  _          -> False 

{-| A função 'eRelva' verifica se uma determinada posição é relva.

-}

eRelva :: Posicao -> Mapa -> Bool
eRelva (x,y) mapa = case procuraTerreno (x,y) mapa of 
  Just Relva -> True 
  _          -> False 

{-| A função 'eAgua' verifica se uma determinada posição é água.

-}

eAgua :: Posicao -> Mapa -> Bool
eAgua (x,y) mapa = case procuraTerreno (x,y) mapa of 
  Just Agua -> True 
  _         -> False 

{-| A função 'procuraTerreno' devolve o tipo de terreno que está sobre uma determinada posição. 

-}

procuraTerreno :: Posicao -> Mapa -> Maybe Terreno  
procuraTerreno (x, y) mapa =
  if x1 >= 0 && x1 < length mapa && y1 >= 0 && y1 < length (head mapa)
    then Just ((mapa !! x1) !! y1)
    else Nothing
  where
    x1 = floor y -- Índice da linha (Y)
    y1 = floor x -- Índice da coluna (X)



{-| A função 'peloMenosUmPortal' verifica se existe pelo menos um portal 

-}

peloMenosUmPortal:: [Portal] -> Bool
peloMenosUmPortal ps = not (null ps)

{-| A função 'validaPosicaoPortal' verifica se todos os portais existentes no jogo estão posicionados sobre a Terra

-}

validaPosicaoPortal :: [Portal] -> Mapa -> Bool
validaPosicaoPortal [] _ = True
validaPosicaoPortal (p:ps) mapa =
  eTerra (posicaoPortal p) mapa && validaPosicaoPortal ps mapa

{-| A função 'naoSobrepostoBasePortal' verifica se a base está sobreposta à algum portal. 
 Como para um bom funcionamento do jogo, a base não pode está sobreposta à nenhum portal, a função devolve *True* quando 
 não houver nenhum portal na mesma posição da base.

-}

naoSobrepostoBasePortal :: Base -> [Portal] -> Bool 
naoSobrepostoBasePortal _ [] = True 
naoSobrepostoBasePortal b ps = not $ elem (posicaoBase b) pps 
  where pps = map posicaoPortal ps 

{-| A função 'naoSobrepostoTorrePortal' verifica se existe alguma torre sobreposta com algum portal. 
 Como para um bom funcionamento do jogo, não pode haver torres sobrepostas com portais, a função devolve *True* quando 
 não houver nenhuma torre na mesma posição de um portal.

-}

naoSobrepostoTorrePortal :: [Torre] -> [Portal] -> Bool
naoSobrepostoTorrePortal [] _ = True 
naoSobrepostoTorrePortal (t:ts) ps = not ((posicaoTorre t) `elem` pps) && naoSobrepostoTorrePortal ts ps
  where pps = map posicaoPortal ps 


{-| A função 'inimigosInicio' verifica o estado dos inimigos inicialmente. Isto é todos os inimigos inicialmente devem ter:
 1. Posição do respetivo portal. 
 2. Nı́vel de vida positivo. 
 3. Lista de projéteis ativos vazia.

-}

inimigosInicio :: [Inimigo] -> [Portal] -> Bool
inimigosInicio [] _ = True 
inimigosInicio (i:is) ps = all (> 0) vi && 
  elem (posicaoInimigo i) pps &&  
  null (projeteisInimigo i) && 
  inimigosInicio is ps
     where pps = map posicaoPortal ps 
           vi = map vidaInimigo (i:is)

{-| A função 'inimigosTerra' verifica se os inimigos encontram-se sempre sobre a terra.

== Nota: 
 Os inimigos só podem andar sobre a terra. Logo, esta função averigua se os inimigos estão sobre a terra. 

-}

inimigosTerra :: [Inimigo] -> Mapa -> Bool
inimigosTerra [] _ = True 
inimigosTerra (i:is) mapa = eTerra (posicaoInimigo i) mapa && inimigosTerra is mapa 

{-| A função 'inimigosTorre' verifica se os inimigos estão sobrepostos a alguma torre. 
 Como os inimigos não podem estar sobrepostos a torres, a função devolve *True* se as posições forem diferentes.

-}

inimigosTorre :: [Inimigo] -> [Torre] -> Bool 
inimigosTorre [] _ = True
inimigosTorre _ [] = True
inimigosTorre (i:is) torres =
  not (any (\t -> posicaoInimigo i == posicaoTorre t) torres) && inimigosTorre is torres

{-| A função 'velocidadeInimigos' verifica se a velocidade dos inimigos é sempre positiva. 

-}   

velocidadeInimigos :: [Inimigo] -> Bool
velocidadeInimigos [] = True
velocidadeInimigos is = all (>0) (map velocidadeInimigo is)  

{- | A função 'normalizaInimigos' verifica se a lista de projéteis ativos encontram-se “normalizada”, ou seja:
       1. Não pode conter mais do que um projétil do mesmo tipo;
       2. Não pode conter, simultaneamente, projéteis do tipo Fogo e Resina, nem Fogo e Gelo.

-}

normalizaInimigos :: [Inimigo] -> Bool
normalizaInimigos is = all normalizainimigo is
  where 
    projetilIgual :: [Projetil] -> Bool
    projetilIgual [] = True 
    projetilIgual (p:ps) = length ps == length (foldr (\x ac -> if x `elem` ac then ac else x:ac) [] (p:ps))
    
    projeteisValidos :: [TipoProjetil] -> Bool
    projeteisValidos tps =
      let contemFogo = Fogo `elem` tps
          contemResina = Resina `elem` tps
          contemGelo = Gelo `elem` tps
      in not (contemFogo && contemResina) && not (contemFogo && contemGelo)

   
    normalizainimigo :: Inimigo -> Bool
    normalizainimigo i = projeteisValidos tProjetil && projetilIgual projetil 
      where projetil = projeteisInimigo i 
            tProjetil = map tipoProjetil (projetil)  



{-| A função 'existePeloMenosUmCaminho' verifica se existe pelo menos um caminho um caminho (de terra) ligando um portal à base. 
 De outra forma, não seria possı́vel a base sofrer dano.
-}
existePeloMenosUmCaminho :: Mapa -> Portal -> Base -> Bool
existePeloMenosUmCaminho mapa p b =
  let posP = posicaoPortal p
      posB = posicaoBase b
  in fst (atualizaPos mapa posP posB [])

eFronteiras :: Posicao -> Bool
eFronteiras (x,y) = x < 0 || y < 0 || x>=6 || y >= 6

jaPassou :: Posicao -> [Posicao] -> Bool
jaPassou _ [] = False
jaPassou (x,y) ((x1,y1):t) = (x==x1 && y == y1) || jaPassou (x,y) t

chegouBase :: Posicao -> Posicao -> Bool
chegouBase (px,py) (bx,by) = px == bx && py == by

verificaDirecaoTerra :: Mapa -> Posicao -> [Posicao] -> Direcao -> Bool
verificaDirecaoTerra m (x,y) lpos d = case d of
  Norte -> not (jaPassou (x,y-1) lpos) && not (eFronteiras (x,y-1)) && eTerra (x,y-1) m
  Sul -> not (jaPassou (x,y+1) lpos) && not (eFronteiras (x,y+1)) && eTerra (x,y+1) m
  Este -> not (jaPassou (x+1,y) lpos) && not (eFronteiras (x+1,y)) && eTerra (x+1,y) m
  Oeste -> not (jaPassou (x-1,y) lpos) && not (eFronteiras (x-1,y)) && eTerra (x-1,y) m

atualizaPos :: Mapa -> Posicao -> Posicao -> [Posicao] -> (Bool, [Posicao])
atualizaPos m pos@(x,y) posB lpos
  | chegouBase pos posB = (True, lpos)
  | verificaDirecaoTerra m pos lpos Norte = atualizaPos m (x,y-1) posB (lpos++[(x,y)])
  | verificaDirecaoTerra m pos lpos Sul = atualizaPos m (x,y+1) posB (lpos++[(x,y)])
  | verificaDirecaoTerra m pos lpos Este = atualizaPos m (x+1,y) posB (lpos++[(x,y)])
  | verificaDirecaoTerra m pos lpos Oeste = atualizaPos m (x-1,y) posB (lpos++[(x,y)])
  | otherwise = (False, lpos)


{-| A função 'validaPosicoesTorres' verifica se, numa lista de torres, todas estão posicionadas sobre terra.
-}
validaPosicoesTorres :: [Torre] -> Mapa -> Bool
validaPosicoesTorres ts m =
  let pts = map posicaoTorre ts
  in aux pts m
    where
      aux [] _ = True
      aux (pt:pts) m
        | not $ eTerra pt m = False
        | otherwise = aux pts m

{-| A função 'alcanceTorresPositivo' verifica se, numa lista de torres, o alcance de todas é um valor positivo.
-}
alcanceTorresPositivo :: [Torre] -> Bool
alcanceTorresPositivo [] = True
alcanceTorresPositivo (t:ts) = alcanceTorre t > 0 && alcanceTorresPositivo ts

{-| A função 'rajadaTorresPositivo' verifica se, numa lista de torres, a rajada é um valor positivo.
-}
rajadaTorresPositivo :: [Torre] -> Bool
rajadaTorresPositivo [] = True
rajadaTorresPositivo (t:ts) = rajadaTorre t > 0 && rajadaTorresPositivo ts


{-| A função 'cicloTorresNaoNegativo' verifica se, numa lista de torres, o ciclo é um valor não negativo.
-}
cicloTorresNaoNegativo :: [Torre] -> Bool
cicloTorresNaoNegativo [] = True
cicloTorresNaoNegativo (t:ts) = cicloTorre t >= 0 && cicloTorresNaoNegativo ts


{-| A função 'naoSobrepostoTorres' verifica se, numa lista de torres, nenhuma está sobreposta a outra. Devolve False caso haja sobreposição.
-}
naoSobrepostoTorres :: [Torre] -> Bool
naoSobrepostoTorres [] = True
naoSobrepostoTorres (t:ts) =
  let pts = map posicaoTorre ts
  in if elem (posicaoTorre t) pts 
      then False 
      else naoSobrepostoTorres ts

{-| A função 'validaPosicaoBase' verifica se uma base tem uma posição válida.
-}
validaPosicaoBase :: Base -> Mapa -> Bool
validaPosicaoBase b m = eTerra (posicaoBase b) m

{-| A função 'creditoNaoNegativoBase' verifica se uma base não tem crédito negativo.
-}
creditoNaoNegativoBase :: Base -> Bool
creditoNaoNegativoBase b = creditosBase b >= 0

{-| A função 'naoSobrepostoBaseTorrePortal' verifica se uma base não está sobreposta a uma torre ou a um portal. Devolve False se houver sobreposição.
-}
naoSobrepostoBaseTorrePortal :: Base -> [Torre] -> [Portal] -> Bool
naoSobrepostoBaseTorrePortal b ts ps = naoSobrepostoBasePortal b ps && sobrepostoBaseTorres b ts
  where
    sobrepostoBaseTorres :: Base -> [Torre] -> Bool
    sobrepostoBaseTorres b ts = not $ elem (posicaoBase b) pts
      where pts = map posicaoTorre ts