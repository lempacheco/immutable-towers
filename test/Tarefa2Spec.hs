
module Tarefa2Spec (testesTarefa2) where
  
import Tarefa2 
import LI12425

import Test.HUnit

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [teste1]


torreT :: Torre 
torreT = Torre {posicaoTorre = (0.5,2.5), 
                alcanceTorre = 2}

inimigo1T :: Inimigo
inimigo1T = Inimigo {posicaoInimigo = (1.5,1.5),
                     direcaoInimigo = Sul, 
                     vidaInimigo = 10 
                    }

inimigo2T :: Inimigo 
inimigo2T = Inimigo {posicaoInimigo = (2.5,4.5),
                     direcaoInimigo = Este, 
                     vidaInimigo = 10 
                    }


teste1 :: Test 
teste1 = 
  TestLabel "Testes para função inimigosNoAlcance" $
    test 
      [ "Não há inimigos no alcance" ~: [] ~=? inimigosNoAlcance torreT [inimigo2T],
        "Há algum inimigo no alcance" ~: [inimigo1T] ~=? inimigosNoAlcance torreT [inimigo1T, inimigo2T]
      ] 