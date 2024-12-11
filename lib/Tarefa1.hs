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

--TODO: atualizar existePeloMenosUmCaminho com as funcoes eTerra,eAgua e eRelva
module Tarefa1 where
import LI12425

-- | A função 'validaJogo' verifica se um jogo é válido
validaJogo :: Jogo -> Bool
validaJogo = undefined

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
procuraTerreno (x,y) mapa = Just $ (mapa !! (fst cse)) !! (snd cse) 
  where cse = (x1,y1)
        x1 = floor x 
        y1 = floor y 

validaTorre :: Torre -> Bool 
validaTorre t = 
    alcanceTorre t > 0 
      && rajadaTorre t > 0 

{-| A função 'peloMenosUmPortal' verifica se existe pelo menos um portal 

-}

peloMenosUmPortal:: [Portal] -> Bool
peloMenosUmPortal ps = not (null ps)

{-| A função 'validaPosicaoPortal' verifica se todos os portais existentes no jogo estão posicionados sobre a Terra

-}

validaPosicaoPortal :: [Portal] -> Mapa -> Bool
validaPosicaoPortal [] _ = True
validaPosicaoPortal (p:ps) mapa = let t = terrenoPortal p mapa
                                  in if t == Just Terra then True && validaPosicaoPortal ps mapa else False

{-| A função 'terrenoPortal' informa qual o terreno que se encontra um portal. 
 Esta é utilizada como função auxiliar na função 'validaPosicaoPortal'  

-}

terrenoPortal :: Portal -> Mapa -> Maybe Terreno 
terrenoPortal p mapa = let cse = (floor (fst (posicaoPortal p)), floor (snd (posicaoPortal p)))  
                      in Just $ (mapa !! (fst cse)) !! (snd cse) 


{-| A função 'sobrepostoBasePortal' verifica se a base está sobreposta à algum portal. 
 Como para um bom funcionamento do jogo, a base não pode está sobreposta à nenhum portal, a função devolve *True* quando 
 não houver nenhum portal na mesma posição da base.

-}

sobrepostoBasePortal :: Base -> [Portal] -> Bool 
sobrepostoBasePortal b ps = not $ elem (posicaoBase b) pps 
  where pps = map posicaoPortal ps 

{-| A função 'sobrepostoTorrePortal' verifica se existe alguma torre sobreposta com algum portal. 
 Como para um bom funcionamento do jogo, não pode haver torres sobrepostas com portais, a função devolve *True* quando 
 não houver nenhuma torre na mesma posição de um portal.

-}

sobrepostoTorrePortal :: [Torre] -> [Portal] -> Bool
sobrepostoTorrePortal [] _ = False 
sobrepostoTorrePortal (t:ts) ps = not $ elem (posicaoTorre t) pps || sobrepostoTorrePortal ts ps
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
 Como os inimigos não podem estar sobrepostos a torres, a função devolve *True* se as posições forem diferentes, 

-}

inimigosTorre :: [Inimigo] -> [Torre] -> Bool 
inimigosTorre [] _ = True
inimigosTorre (i:is) (t:ts) = not (posicaoInimigo i == posicaoTorre t && inimigosTorre is ts)

{-| A função 'velocidadeInimigos' verifica se a velocidade dos inimigos é sempre positiva. 

-}   

velocidadeInimigos :: [Inimigo] -> Bool
velocidadeInimigos [] = True
velocidadeInimigos is = all (>0) (map velocidadeInimigo is)  

--(e) A lista de projéteis ativos deve encontrar-se “normalizada”, ou seja:
-- i. Não pode conter mais do que um projétil do mesmo tipo.
-- ii. Não pode conter, simultaneamente, projéteis do tipo Fogo e Resina
-- nem Fogo e Gelo (ver secção 3.2 para mais detalhes).

{-| A função 'existePeloMenosUmCaminho' verifica se existe pelo menos um caminho um caminho (de terra) ligando um portal à base. De outra forma, não seria possı́vel a base sofrer dano.
-}
existePeloMenosUmCaminho :: Mapa -> Portal -> Base -> Bool
existePeloMenosUmCaminho mapa p b =
  let posP = posicaoPortal p
      posB = posicaoBase b
  in fst (atualizaPos mapa posP posB [])
    where
      eFronteiras :: Posicao -> Bool
      eFronteiras (x,y) = x < 0 || y < 0 || x>=6 || y >= 6

      eTerra :: Mapa -> Posicao -> Bool
      eTerra m (x,y) = (m !! floor x) !! floor y == Terra

      jaPassou :: Posicao -> [Posicao] -> Bool
      jaPassou _ [] = False
      jaPassou (x,y) ((x1,y1):t) = (x==x1 && y == y1) || jaPassou (x,y) t

      chegouBase :: Posicao -> Posicao -> Bool
      chegouBase (px,py) (bx,by) = px == bx && py == by

      verificaDirecaoTerra :: Mapa -> Posicao -> [Posicao] -> Direcao -> Bool
      verificaDirecaoTerra m (x,y) lpos d = case d of
        Norte -> not (jaPassou (x,y-1) lpos) && not (eFronteiras (x,y-1)) && eTerra m (x,y-1)
        Sul -> not (jaPassou (x,y+1) lpos) && not (eFronteiras (x,y+1)) && eTerra m (x,y+1)
        Este -> not (jaPassou (x+1,y) lpos) && not (eFronteiras (x+1,y)) && eTerra m (x+1,y)
        Oeste -> not (jaPassou (x-1,y) lpos) && not (eFronteiras (x-1,y)) && eTerra m (x-1,y)

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
validaPosicoesTorres [] _ = True
validaPosicoesTorres (t:ts) mapa = if terrenoTorre t mapa == Just Relva then validaPosicoesTorres ts mapa else False
  where
    terrenoTorre torre m = let cse = (floor (fst (posicaoTorre torre)), floor (snd (posicaoTorre torre)))
                      in Just $ (m !! fst cse) !! snd cse


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

cicloTorresNaoNegativo :: [Torre] -> Bool
cicloTorresNaoNegativo [] = True
cicloTorresNaoNegativo (t:ts) = cicloTorre t >= 0 && cicloTorresNaoNegativo ts

sobrepostoTorres :: [Torre] -> Bool
sobrepostoTorres [] = False
sobrepostoTorres (t:ts) = (foldr (||)False $ map (\x -> posicaoTorre t == posicaoTorre x) ts) || sobrepostoTorres ts

validaPosicaoBase :: Base -> Mapa -> Bool
validaPosicaoBase b m = terrenoBase b m == Just Terra
  where 
    terrenoBase base mapa =
      let cse = (floor (fst (posicaoBase base)), floor (snd (posicaoBase base)))
      in Just $ (mapa !! fst cse) !! snd cse

creditoNaoNegativoBase :: Base -> Bool
creditoNaoNegativoBase b = creditosBase b >= 0

sobrepostoBaseTorrePortal :: Base -> [Torre] -> [Portal] -> Bool
sobrepostoBaseTorrePortal b ts ps = sobrepostoBasePortal b ps || sobrepostoBaseTorres b ts

sobrepostoBaseTorres :: Base -> [Torre] -> Bool
sobrepostoBaseTorres b ts = elem (posicaoBase b) pts
  where pts = map posicaoTorre ts

--para propositos de debugging
mapa1 :: Mapa
mapa1 =
 [ [t, t, r, a, a, a],
   [r, t, r, a, r, r],
   [r, t, r, a, r, t],
   [r, t, r, a, r, t],
   [r, t, t, t, t, t],
   [a, a, a, a, r, r]
 ]
 where
       t = Terra
       r = Relva
       a = Agua

portal1 :: Portal
portal1 = Portal {posicaoPortal = (0.5,0.5)}

base1 :: Base
base1 = Base {posicaoBase = (2.5,5.5)}

mapa2 :: Mapa
mapa2 =
 [ [t, t, r, a, a, a],
   [r, t, r, a, r, r],
   [r, a, r, a, r, t],
   [r, t, r, a, r, t],
   [r, t, t, t, t, t],
   [a, a, a, a, r, r]
 ]
 where
       t = Terra
       r = Relva
       a = Agua

mapa3 :: Mapa
mapa3 =
 [ [t, a],
   [t, t]
 ]
 where
       t = Terra
       --r = Relva
       a = Agua

base3 :: Base
base3 = Base {posicaoBase = (1.5,1.5)}

mapa4 :: Mapa
mapa4 =
 [ [t, t, r, a, a, a],
   [t, t, t, t, r, r],
   [t, t, r, a, r, t],
   [t, t, r, a, r, t],
   [t, t, t, t, t, t],
   [t, a, a, a, r, r]
 ]
 where
       t = Terra
       r = Relva
       a = Agua

torre1 :: Torre
torre1 = Torre {posicaoTorre = (1.5,5.5)}

torre2 :: Torre
torre2 = Torre {posicaoTorre = (2.5,5.5)}

torre3 :: Torre
torre3 = Torre {posicaoTorre = (0.5,2.5)}
