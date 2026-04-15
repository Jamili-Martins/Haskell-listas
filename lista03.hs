--  1) Como a versão recursiva da função fatorial se comporta se dermos a ela como argumento um número negativo? Modifique a implementação clássica para não permitir números negativos adicionando uma guarda ao passo recursivo.

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n 
    | n > 0 = n * fatorial (n-1)
    | otherwise = error "Fatorial não definido para números negativos"

-- Passar um número negativo, faz o código quebrar.


-- 2) Defina a função recursiva somar :: Int -> Int que retorna a soma dos inteiros não-negativos a partir de um valor até zero. Por exemplo, somar 3 deve retornar 3+2+1+0 = 6.

soma :: Int -> Int
soma 0 = 0
soma n = n + soma (n - 1)


{- 3) Defina o operador de exponenciação ^ utilizando uma função recursiva, semelhante ao padrão usado para implementar a multiplicação com o operador *:
(*) :: Num a => a -> a -> a
m * 0 = 0
m * n = m + (m * (n - 1)) -}

potencia :: Num a => a -> Int -> a
potencia e 0 = 1
potencia e x = e * ( potencia e (x - 1))


{- 4) Defina a função euclides :: Int -> Int -> Int que implementa o algoritmo de Euclides para calcular o máximo divisor comum de dois inteiros não-negativos: se dois números são iguais,
este número é o resultado; caso contrário, o menor número é subtraído do maior e o processo é repetido passando este novo número e o menor valor passado anteriormente como argumento. Exemplo:
> euclides 6 27
3 -}

euclides :: Int -> Int -> Int
euclides a b
    | a == b = b                     -- Se forem iguais o resultado é o próprio número
    | a > b = euclides (a-b) b       -- Se a  for maior que b, subtrai(a-b) e tenta de novo
    | otherwise = euclides a (b-a)   -- Se b for maior que a , subtrai(b-a) e tenta de novo


{- 5. Defina as funções abaixo usando recursão:
 a) Decidir se todos os valores em uma lista são True:
and :: [Bool] -> Bool -}

meuAnd :: [Bool] -> Bool
meuAnd [] = True                   
meuAnd (x:xs) = x && meuAnd xs       -- Pegamos o primeiro valor (x) e fazemos uma comparação lógica "E" com o que vier depois


{- b) Concatenar uma lista de listas:
concat :: [[a]] -> [a] -}

meuConcat :: [[a]] -> [a]
meuConcat [] = []
meuConcat (x:xs) = x ++ meuConcat xs


{- c) Produzir uma lista com n elementos idênticos:
replicate :: Int -> a -> [a] -}

meuReplicate :: Int -> a -> [a]
meuReplicate 0 _ = []
meuReplicate n x = x : meuReplicate (n - 1) x


{- d) Selecionar o n-ésimo elemento em uma lista:
(!!) :: [a] -> Int -> a -}

busca :: [a] -> Int -> a
busca (x:_) 0 = x
busca (_:xs) n = busca xs (n-1)

{- e) Decidir se um valor está presente em uma lista:
elem :: Eq a => a -> [a] -> Bool -}


estaPresente :: Eq a => a -> [a] -> Bool
estaPresente _ [] = False
estaPresente n (x:xs)
    | n == x = True
    | otherwise = estaPresente n xs     -- Verifica se um elemneto está na lista ou não


{- 6) Definir a função recursiva merge :: Ord a => [a] -> [a] -> [a] que une duas listas ordenadas em uma lista ordenada. Exemplo:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6] -}

listaOrd :: Ord a => [a] -> [a] -> [a]
listaOrd [] ys = ys
listaOrd xs [] = xs
listaOrd (x:xs) (y:ys)
    | x < y = x : listaOrd xs (y:ys)
    | otherwise = y : listaOrd (x:xs) ys


{- 7) Usando a função merge, defina a função mergesort :: Ord a => [a] -> [a] que implementa o algoritmo de ordenação 
merge sort, que por sua vez considera uma lista vazia e uma lista com apenas um elemento como listas ordenadas, e que 
qualquer outra lista é ordenada a partir da união de duas listas que resultaram da ordenação das suas metades separadamente. Dica: primeiro implemente a função metades :: [a] -> ([a],[a]) que separa uma lista em duas partes cujos comprimentos são iguais ou no máximo diferem em uma unidade.-}

metades :: [a] -> ([a], [a])  -- Devolve as tuplas
metades xs = splitAt (length xs `div` 2) xs  -- A função splitAt corta uma lista em um índice específico e nos devolve os dois pedaços dentro de uma tupla.


merge :: Ord a => [a] -> [a] -> [a]  
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)  -- Se x fou igual ou menos ele vai para o início da nova lista, a função é chamada novamente usando o que sobrou da primeira lista (xs) contra a segunda lista inteira (y:ys)
    | otherwise = y : merge (x:xs) ys  -- Quando o y for menor ele "ganha" e vai parar a frente e a função compara a lista inteira (x:xs) com o que sobrou da segunda (ys)


mergesort :: Ord a => [a] -> [a]
-- Casos base
mergesort [] = []
mergesort [x] = [x] -- Uma lista com um único elemento já está ordenada por natureza

-- Caso geral
mergesort xs = merge (mergesort esq) (mergesort dir)
    where (esq, dir) = metades xs  -- O where nomeia as tuplas como esquerda e direita
                                  -- A função metades entrega a tupla: (esq, dir) ainda bagunçadas.
                                  -- O mergesort chama a si mesmo para ordenas as metades.
                                  -- O merge recebe as metades já ordenadas e as juntas de forma inteligente.


{- 8) Implemente recursivamente funções que:
 a) calcule a soma de uma lista de inteiros; -}                  

somaInt :: [Int] -> Int
somaInt [] = 0
somaInt (x:xs) = x + somaInt xs  --Pega o primeiro e "chama" a função pro resto

{- b) obtenha o número de elementos de uma lista; -}

tamanhoList :: [Int] -> Int
tamanhoList [] = 0
tamanhoList (_:xs) = 1 + tamanhoList xs  -- o underscore significa que eu não me importo com o valor, mas sei que ele tá ali
                                         -- Vai somando 1 para cada elemento da lista


{- c) selecione o último elemento de uma lista não-vazia. -}

ultElemento :: [a] -> a
ultElemento [x] = x
ultElemento (x:xs) = ultElemento xs