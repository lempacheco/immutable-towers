module FuncoesAux where

import LI12425

import Data.List ( sortBy ) 
import Data.Ord (comparing)

-- Bases do jogo

baseTds :: Base
baseTds = Base {vidaBase = 100,
                creditosBase = 200} 

-- inimigos Homem
inimigo1Tds :: Inimigo
inimigo1Tds = Inimigo {tipoInimigo = Homem, 
                        projeteisInimigo = [], 
                        vidaInimigo = 150, 
                        butimInimigo = 50, 
                        ataqueInimigo = 40, 
                        velocidadeInimigo = 0.5, 
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1}

-- inimigos Mulher
inimigo2Tds :: Inimigo
inimigo2Tds = Inimigo {tipoInimigo = Mulher, 
                        projeteisInimigo = [], 
                        vidaInimigo = 100, 
                        butimInimigo = 45,  
                        ataqueInimigo = 20, 
                        velocidadeInimigo = 1,
                        caminhoInimigo = [],
                        iteracoesDesdeInicioAnimacaoInimigo = 1}

geraOndasPortal :: Int -> Int -> Int -> Posicao -> [Onda]
geraOndasPortal 0 _ _ _ = []
geraOndasPortal qOndas n1 n2 posP = 
  let ondas = geraOndaPortal n1 n2 posP : geraOndasPortal (qOndas-1) n1 n2 posP
  in (last ondas) {tempoOnda = 0} : init ondas

{-| Cria uma onda de inimigos com base nos parâmetros fornecidos

-}

geraOndaPortal :: Int -- ^ quantidade de inimigos masculinos. 
               -> Int -- ^ quantidade de inimigos femininos. 
               -> Posicao -- ^ posição inicial dos inimigos. 
               -> Onda -- ^ Onda com a configuração definida.
geraOndaPortal n1 n2 posP = 
  let is1 = geraIs1 posP n1
      is2 = geraIs2 posP n2
  in Onda {inimigosOnda = juntaIs1Is2 is1 is2 0, 
            cicloOnda = 5*60,
            tempoOnda = 10*60,
            entradaOnda = 0
            }

{-| Cria um grupo com n inimigos masculinos. 

-}

geraIs1 :: Posicao -> Int -> [Inimigo]
geraIs1 posP n1
  | n1 == 0 = []
  | otherwise = inimigo1Tds {posicaoInimigo = posP, acDirecao = posP} : geraIs1 posP (n1-1)

{-| Cria um grupo com n inimigos femininos. 

-}

geraIs2 :: Posicao -> Int -> [Inimigo]
geraIs2 posP n2
  | n2 == 0 = []
  | otherwise = inimigo2Tds {posicaoInimigo = posP, acDirecao = posP} : geraIs2 posP (n2-1)

{-| Cria um grupo de inimigos, intercalando os masculinos com os femininos. 

-}

juntaIs1Is2 :: [Inimigo] -> [Inimigo] -> Int -> [Inimigo]
juntaIs1Is2 [] is2 _ = is2
juntaIs1Is2 is1 [] _ = is1
juntaIs1Is2 is1 is2 ac
  | mod ac 2 == 0 = head is1 : juntaIs1Is2 (tail is1) is2 (ac+1)
  | otherwise = head is2 : juntaIs1Is2 is1 (tail is2) (ac+1)

{-| Atualiza uma lista de terrenos. Caso a posição já exista, o terreno é atualizado. Caso contrário, a nova posição 
e o terreno são adicionados a lista.

==__Exemplo de utilização:__

>>> let lt = [((0,0), Terra), ((1,1), Agua)]

>>> atualizaMapa ((1,1), Terra) lt 
[((0,0), Terra), ((1,1), Terra)]

-}

atualizaMapa :: (Posicao, Terreno) -> [(Posicao,Terreno)] -> [(Posicao, Terreno)]
atualizaMapa (pos, ter) [] = [(pos,ter)]
atualizaMapa (pos, ter) lt = (pos, ter): filter (\(p,_) -> p /= pos) lt 

{-| Converte uma lista de terrenos com a devida posição, em um mapa ([[Terreno]])

==__Nota:__
Caso o terreno não seja definido, será preenchido com 'Relva'. 

-}

transformaMapa :: [(Posicao, Terreno)] -> Mapa
transformaMapa lt =
    [[procuraTerrenoNaLista (x, y) lt | x <- [0..15]] | y <- [0..15]]
  where
    procuraTerrenoNaLista :: Posicao -> [(Posicao, Terreno)] -> Terreno
    procuraTerrenoNaLista pos lt =
        case lookup pos lt of
            Just terreno -> terreno
            Nothing -> Relva 

{-| Adiciona um portal em uma lista de portais, caso a sua posição esteja em um terreno 'Terra'. 

-}

adicionarPortais :: Portal -> [(Posicao, Terreno)] -> [Portal] -> [Portal]
adicionarPortais p lt ps
    | lookup pp lt == Just Terra = p : ps
    | otherwise = ps
  where
    pp = posicaoPortal p

{-| Remove um portal de uma lista de portais, se a sua posição for igual a uma determinada posição. 

-}

deletePortal :: [Portal] -> Posicao -> [Portal] 
deletePortal [] _ = []
deletePortal (p:portais) pos   
    | pp == pos = portais 
    | otherwise = p:deletePortal portais pos
  where
    pp = posicaoPortal p

