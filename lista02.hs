-- 1) Usando compreensão de listas, forneça uma expressão que calcula a soma 1² +2²+...+100² dos quadrados dos primeiros 100 números inteiros.

resultado = sum [x^2 | x <- [1..100]]  -- x <- é o gerador. Ele extrai cada número da lista e atribui a variável x.
                           -- Para cada x que o gerador extraiu, o Haskell calculo seu quadrado.
                           -- O sum recebe a listaa do resultado e soma tudo devolvendo um elemento (a soma dos elementos).


{- 2) Suponha que um plano de coordenadas de tamanho m x n é dado pela lista de todos os pares (x,y) de inteiros tal que 0  x  m e 0 y n.
Usando compreensão de listas, defina a função grid :: Int -> Int -> [(Int,Int)] que retorna o plano de coordenadas de um dado tamanho.
Por exemplo:
> grid 1 2
[(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)] -}

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0..m], y <- [0..n]]


{- 3) Usando compreensão de listas e a função grid definida na questão anterior, defina uma função quadrado :: Int -> [(Int,Int)] 
que retorna um plano de coordenadas quadrado de tamanho n, excluindo a diagonal principal (0,0) a (n,n). Por exemplo:
> quadrado 2
[(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)] -}

quadrado :: Int -> [(Int, Int)]
quadrado n = [ (x, y) | (x, y) <- grid n n, x /= y ] -- x /= y  age como filtro. Tipo, (0,0) são diferentes? Não. (Descarta). Ou, (0,1) são diferentes? Sim. (Mantém)


{- 4) De maneira similar à função length, mostre como a função replicate :: Int -> a -> [a] que produz uma lista de elementos idênticos
pode ser definida usando compreensão de listas. Exemplo:
> replicate 3 True
[True, True, True]-}

meuReplicate :: Int -> a -> [a]
meuReplicate n x = [ x | _ <- [1..n]]   -- O n será a quantidade de vezes que x será repetixo. e x o valor a ser repetido.
                                   -- Se usa o undescore porque você não precisa do valor do número.


{- 5) Uma tupla (x,y,z) de inteiros positivos é Pitagoreana se satisfaz a equação x² + y² = z². Usando compreensão de listas com
três geradores, defina a função pitag :: Int -> [(Int, Int, Int)] que retorna uma lista de todas as tuplas que satisfazem a condição
estabelecida e cujos componentes são menores ou iguais a um dado limite. Exemplo: 
> pitag 10
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)] -}

pitag :: Int -> [(Int, Int, Int)]
pitag n = [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


{- 6) Um inteiro positivo é perfeito se ele é igual à soma de todos os seus fatores, excluindo o próprio número. Usando compreensão de listas e a função fatores,
defina a função perfeitos :: Int -> [Int] que retorna a lista de todos os números perfeitos menores que um limite informado como argumento. Exemplo:
> perfeitos 500
[6, 28, 496] -}


fatores :: Int -> [Int]
fatores n = [x | x <- [1..n-1], n `mod` x == 0]  -- n-1 porque o enunciado pede para ecluir o próprio número.
                                                 -- um número é fator de outro se o resto for 0.



perfeitos :: Int -> [Int]
perfeitos n = [ x | x <- [1..n], sum (fatores x) == x]   -- == x: O filtro. O Haskell compara: "A soma que acabei de calcular é igual ao próprio número x?".



{- 7) Mostre que a compreensão de lista [(x,y) | x <- [1,	2], y <- [3,4]], com dois geradores,
pode ser representada usando duas compreensões de lista, cada uma com apenas um gerador. Dica: Procure usar a função concat. -}

func :: [(Int, Int)]
func = concat [ [(x, y) | y <- [3, 4]] | x <- [1,2]]  -- O concat "achata" essa estrutura, transformando-a em uma lista única de tuplas

{- 8) Redefina a função posicoes usando a função buscar, disponível em
https://emanoelbarreiros.github.io/funcional/haskell-5#a-fun%C3%A7%C3%A3o-zip -}


buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']  

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = buscar x (zip xs [0..n])  -- O zip  numera a lista, tipo: [('A',0), ('B',1), ('A',2)] 
    where n = length xs -1                -- A função posicoes entrega essa lista numerada para a função buscar
                                          -- A função posicoes entrega essa lista numerada para a função buscar



{- 9) Escreva a função capaz de calcular o produto escalar de duas listas de inteiros xs e ys de tamanho n, que é dado
pelo produto dos inteiros em posições correspondentes:
i=0n-1(xsi*ysi)
Dica: Procure usar a função zip. -}

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum [x * y | (x, y) <- zip xs ys] --zip xs ys: Esta função "grampeia" as duas listas. Se você tiver xs = [1, 2, 3] e ys = [4, 5, 6], o zip transforma isso em uma lista de pares: [(1,4), (2,5), (3,6)].