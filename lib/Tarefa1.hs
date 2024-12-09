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

-- Exemplos para teste 
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

inimigo1 :: Inimigo 
inimigo1 = Inimigo {posicaoInimigo = (0.5,0.5),
    direcaoInimigo = Norte,
    vidaInimigo = 5,
    velocidadeInimigo = 4.5,
    projeteisInimigo = []}

inimigo2 :: Inimigo 
inimigo2 = Inimigo {posicaoInimigo = (1.5,1.5),
    direcaoInimigo = Sul,
    vidaInimigo = 5,
    velocidadeInimigo = 4.5,
    projeteisInimigo = []}

torre1 :: Torre 
torre1 = Torre {posicaoTorre = (0.3,0.3)}

base1 :: Base 
base1 = Base {posicaoBase = (1.5,1.5)}


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

