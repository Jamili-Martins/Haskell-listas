import Data.Char (digitToInt)  -- O digitToInt é o tradutor. Ele pega o caractere '4' e o transforma no número real 4. (Para a questão 21)

-- 1) Fornecidos três valores a, b e c, escreva uma função que retorne quantos dos três são iguais. A resposta pode ser 3 (todos iguais), 2 (dois iguais e o terceiro diferente) ou 0 (todos diferentes).

quantosIguais :: Int -> Int -> Int -> Int
quantosIguais a b c
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0

-- 2) Fornecidos três valores a, b e c, elaborar uma função que retorne quantos desses três valores são maiores que a média entre eles.

quantosMaioresQueMedia :: Float -> Float -> Float -> Int
quantosMaioresQueMedia a b c =
    (if a > m then 1 else 0) +
    (if b > m then 1 else 0) +
    (if c > m then 1 else 0)
    where m = (a + b + c) / 3


-- 3) Escreva uma função potencia_2 que retorne o quadrado de um número (x2).

potencia_2 :: Int -> Int
potencia_2 x = x*x


-- 4) Reutilizando a função potencia_2, construir uma função potencia_4 que retorne o seu argumento elevado à quarta potência.

potencia_4 ::Int -> Int
potencia_4 x = potencia_2 (potencia_2 x) -- Calcula o quadrado de x, e depois calcula o quadrado do resultado.


{- 5) Implemente em Haskell a função do ou-exclusivo, que é dada por:
a (x) b = (a v b) ^ ¬(a ^ b) -}

xor :: Bool -> Bool -> Bool
xor a b
    | a == b = False
    | otherwise = True


-- 6) Escrever duas funções, x_maior que retorne o maior e x_menor que retorne o menor valor real, das raízes de uma equação do segundo grau. A expressão genérica é dada por: (Função Quadrática)

x_maior :: Double -> Double -> Double -> Double  --Calculando raízes de uma equação do segundo grau
x_maior a b c = (-b + sqrt delta) / (2*a)
    where delta = b^2 - 4 * a * c  --Definindo o delta


x_menor :: Double -> Double -> Double -> Double
x_menor a b c = (-b - sqrt delta) / (2 * a)
    where delta = b^2 - 4 * a * c


-- 7) Criar funções que calculam a soma dos números entre n1 e n2, incluindo e excluindo os limites.

soma_excluindo :: Int -> Int -> Int
soma_excluindo n1 n2 = sum [n1 + 1..n2 - 1] -- Soma excluindo os limites


-- 8) Dados três números n1, n2 e n3, encontrar os múltiplos de n3 que se encontram no  intervalo [n1,n2] (inclusivo).

multiplos :: Int -> Int -> Int -> [Int]
multiplos n1 n2 n3 = filter (\x -> x `mod` n3 == 0) [n1..n2]


-- 9) Utilizando a função sum, faça uma função que calcule a multiplicação entre dois números quaisquer, considerando números positivos e negativos. Não é permitido usar a operação de multiplicação.

multi :: Int -> Int -> Int
multi x y = sinal * sum [abs x | _ <- [1.. abs y]]
    where
        sinal = if(signum x < 0 && signum y > 0) || (signum x > 0 && y < 0)
                then -1
                else 1


-- 10) Implemente a função mod2, que retorna o resto de uma divisão de inteiros. OBS: não é permitido usar a função mod nem a função rem da biblioteca.

mod2 :: Int -> Int -> Int
mod2 x y
    | x < y     = x             -- Se x é menor que y, o resto é o próprio x
    | otherwise = mod2 (x - y) y -- Caso contrário, subtrai y e chama a função de novo


{- 11) Seja a sequência:
a1 = sqrt 6
a2 = sqrt (6 + sqrt 6)
a3 = sqrt (6 + sqrt (6 + sqrt 6))
a4 = ...
Escreva a função que calcula essa sequência. -}

-- O inteiro é referente a posição de n
-- Retorna um número flutuante
sequencia :: Int -> Double      
sequencia 1 = sqrt 6           -- Caso base: se n for 1, o resultado da raiz é 6
sequencia n = sqrt (6 + sequencia (n-1))  -- Caso geral: n é a raiz de 6 + o termo anterior


-- 12) Implementar a fórmula que indica de quantas maneiras é possível escolher n objetos de uma coleção original de m objetos, onde m >= n.

fatorial :: Int -> Int
fatorial 0 = 1               -- Caso base
fatorial n = n * fatorial (n - 1) -- Regra do fatorial: n! = n * fatorial (n-1)!


combinacao :: Int -> Int -> Int
combinacao m n  = (fatorial m) `div` (fatorial n * fatorial (m - n))  -- div é usado para divisões de números inteiros
           

-- 13) Defina uma função que, dada uma lista numérica, retorne uma tupla, que contenha o maior da lista bem como índice na lista.


maior_indice :: [Int] -> (Int, Int)
maior_indice lista = encontrar_maior (zip lista [0..])  -- A função zip coloca o índice do lado de cada elemento da sua lista automaticamente!


-- Recursão que vai cmoparando as tuplas uma por uma
encontrar_maior :: [(Int, Int)] -> (Int, Int)
encontrar_maior [x] = x -- Se só tem x, x é o maior
encontrar_maior (x:xs) = maior_entre x (encontrar_maior xs)


-- Compara duas tuplas e devolve a que tem o maior valor
maior_entre :: (Int, Int) -> (Int, Int) -> (Int, Int)
maior_entre (v1, i1) (v2, i2)
    | v1 > v2 = (v1, i1)
    | otherwise = (v2, i2)


-- 14) Defina uma função que converta uma lista de dígitos (unitários, 0 a 9) em uma outra lista, que é a sua tradução em String. Considere um dicionário do tipo:


dic_10 = [(0,"zero"), (1,"um"), (2,"dois"), (3,"tres"), (4,"quatro"), 
          (5,"cinco"), (6,"seis"), (7,"sete"), (8,"oito"), (9,"nove")]

converte :: [Int] -> [String]
converte [] = [] -- Caso base: lista vazia vira lista vazia
converte (x:xs) = buscarNome x dic_10 : converte xs   -- Traduz a cabeça e chama o resto


-- Função que busca o nome no dicionário
buscarNome :: Int -> [(Int, String)] -> String
buscarNome n ((valor, nome):resto)
        | n == valor = nome
        |otherwise = buscarNome n resto


-- 15) Construa uma função del_posicao_n :: [Int] -> Int -> [Int] em que dada uma lista de inteiros e a posição de um elemento qualquer, retorne uma nova lista sem aquele elemento da n-ésima posição.

del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n  [] n = []

del_posicao_n (x:xs) 0 = xs
del_posicao_n (x:xs) n = x : del_posicao_n xs (n-1)  -- Quando não é o 0, ele guarda a posção e junta com o resultado


-- 16) Construa uma função inserir_posicao_x :: [Int] -> Int -> Int -> [Int] em que, dada uma lista de inteiros, uma posição e um elemento a ser inserido, retorne uma nova lista com aquele elemento na n-ésima posição.

inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x [] v pos = [v]  -- Se a lista acabar colocamos o valor nela
inserir_posicao_x (x:xs) v 0 = v : (x:xs)  -- Se chegou em 0, colocamos o v na frente da lista atual
inserir_posicao_x (x:xs) v pos = x : inserir_posicao_x xs v (pos - 1)


-- 17) Defina uma função que retorne o valor da n-ésima posição de uma lista.

posicao_x :: [Int] -> Int -> Int
posicao_x [] n = 0
posicao_x (x:xs) 0 = x 
posicao_x (x:xs) n = posicao_x xs (n-1) -- Se a posicção for maior que 0, ele ignora o x e anda na cauda


-- 18) Dadas duas listas ordenadas como entrada, faça uma função merge, que une as duas listas.

merge :: [Int] -> [Int] -> [Int]
merge [] listaB = listaB
merge listaA [] = listaA
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys  -- Aqui o x é maior, então tem que esperar ys ser percorrida



-- 19) Implemente uma função que receba duas listas de inteiros sem repetição, e retorne uma terceira lista que contenha somente números que estejam nas duas listas dadas.

 -- Função para saber seum número n está na lista
pertence :: Int -> [Int] -> Bool
pertence n [] = False
pertence n (x:xs)
        | n == x = True  -- Se o valor n pertence a cabeça, pertence a cauda
        | otherwise = pertence n xs


intersecao :: [Int] -> [Int] -> [Int]
intersecao [] listaB = []   -- Se a primeira lista for vazia, não há oq comparar
intersecao (x:xs) listaB
        -- Escaneia a seegunda lista inteira
        | pertence x listaB = x : intersecao xs listaB   -- Se x e stá na B, ele entra
        | otherwise = intersecao xs listaB  -- Se não está apenas pula o x


{- 20)  Crie uma função que faça uma compressão sobre uma sequência de caracteres iguais, substitua a sequência por !na, onde n é o número de vezes que o caractere a é repetido. Observe que só devem ser comprimidas sequências de tamanhos maiores que 3. Exemplo:

comprime "asdffffghjjkllllpoooi"
"asd!4fghjjk!4lpoooi" -}

comprime :: String -> String
comprime [] = []
comprime (x:xs)
    | qtd > 3   = "!" ++ show qtd ++ [x] ++ comprime resto
    | otherwise = x : comprime xs  -- Trocamos ; por :
    where
        -- Somamos 1 com o tamanho do que o takeWhile achou
        qtd = 1 + length (takeWhile (== x) xs) 
        -- Pega o que sobrou da lista depois dos repetidos
        resto = dropWhile (== x) xs



-- O ++ serve para juntar duas listas inteiras
-- O show pega um valor de qualquer tipo (como o número 4) e o transforma em uma String (texto "4").
{- takeWhile e dropWhile (Os "Pegadores" Inteligentes)
Essas duas são como "filtros de movimento" que olham para a lista enquanto uma condição for verdadeira. -}
-- takeWhile (== x) xs: "Pegue elementos do início enquanto eles forem iguais a x
--dropWhile (== x) xs: "Jogue fora do início enquanto eles forem iguais a x


-- 21) Implemente uma função que descomprima o texto resultante da função anterior.



descomprime :: String -> String
descomprime [] = []

-- Encontrou o padrão de compressão "!na"
descomprime ('!':n:a:xs) = replicate (digitToInt n) a ++ descomprime xs  -- Ffunção para gerar listas repetidas

-- Letra comum (não comprimida)
descomprime (x:xs) = x: descomprime xs
