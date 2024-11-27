{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Letícia Maria de Lima Cavalcanti Pacheco <a112062@alunos.uminho.pt>
              Filipa Raquel Ferreira Peixoto <a107009@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}

module Tarefa1 where
import LI12425

-- | A função 'validaJogo' verifica se um jogo é válido
validaJogo :: Jogo -> Bool
validaJogo = undefined

validaTorre :: Torre -> Bool 
validaTorre t = 
    alcanceTorre t > 0 
      && rajadaTorre t > 0 

{-| A função 'validaPortais' verifica se existe pelo menos um portal 

-}

peloMenosUmPortal:: [Portal] -> Bool
peloMenosUmPortal ps = not (null ps)


mapa01 :: Mapa
mapa01 =
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

validaPosicaoPortal :: [Portal] -> Mapa -> Bool
validaPosicaoPortal [] _ = True 
validaPosicaoPortal (p:ps) mapa = let t = terrenoPortal p mapa
                                  in if t == Just Terra then True && validaPosicaoPortal ps mapa else False 

terrenoPortal :: Portal -> Mapa -> Maybe Terreno 
terrenoPortal p mapa = let cse = (floor (fst (posicaoPortal p)), floor (snd (posicaoPortal p)))  
                      in Just $ (mapa !! (fst cse)) !! (snd cse) 

base1 :: Base 
base1 = Base {posicaoBase = (1.5,1.5)}

sobrepostoBasePortal :: Base -> [Portal] -> Bool 
sobrepostoBasePortal b ps = elem (posicaoBase b) pps 
  where pps = map posicaoPortal ps 

sobrepostoTorrePortal :: [Torre] -> [Portal] -> Bool
sobrepostoTorrePortal [] _ = False 
sobrepostoTorrePortal (t:ts) ps = elem (posicaoTorre t) pps || sobrepostoTorrePortal ts ps
  where pps = map posicaoPortal ps 
  
        