--- Proyecto I ---

--- 1) Programa las siguientes funciones:
--- a)
esCero :: Int -> Bool 
esCero x = x == 0
--- b)
esPositivo :: Int -> Bool
esPositivo x = x > 0
--- c)
esVocal :: Char -> Bool
esVocal a = (a=='a')||(a=='e')||(a=='i')||(a=='o')||(a=='u')

---2) Programar las siguientes funciones usando recursión o composicón.
---a)
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x == True = paratodo xs
                | otherwise= False
--- b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
---c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs
---d)
factorial :: Int -> Int
factorial 0 = 1 
factorial x = x *(factorial (x-1)) 
---e) 
promedio :: [Int] -> Int
promedio [] = 0
promedio (xs) = div (sumatoria xs) (length xs)

---3) Programar la función pertenece :: Int -> [Int] -> Bool, que verifica si un entero pertenece a una lista.

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise= pertenece n xs 

---4) Programar la función encuentra.

encuentra :: Int -> [(Int,String)] -> String
encuentra _ [] = ""
encuentra n ((x,y):xs) | (n == x) = y
                       | otherwise = encuentra n xs

--- 5) Programar las siguientes funciones que implementan cuantificadores generales.
--- a) paratodo'
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True 
paratodo' (x:xs) t = (t x) && (paratodo' xs t)
--- b) existe'
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = True 
existe' (x:xs) t = (t x) || (existe' xs t)
--- c) sumatoria'
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) t = (t x) + (sumatoria' xs t)
--- d) productoria'
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) t = (t x) * (productoria' xs t)

--- 6) Definir nuevamente la función paratodo utilizando paratodo'.

paratodo2 :: [Bool] -> Bool
paratodo2 xs = paratodo' xs id

--- 7) Utilizar las funciones del ejercicio 5 y programar por composición.
--- a) todosPares
todosPares :: [Int] -> Bool 
todosPares xs = paratodo' xs (\x -> (mod x 2 == 0))
--- b) hayMultiplo
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (\x -> (mod x n == 0)) 
--- c) sumaCuadrados. el rango entre n y m es [n..m]
sumaCuadrados :: Int -> Int
sumaCuadrados n = (sumatoria' [0..n] (\x -> (x*x)))
--- d) hacer factorial sin recursión.
factorial2  :: Int -> Int 
factorial2 n = productoria [1..n]
--- e) multiplicaPares
multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' xs (\x -> (if mod x 2 == 0 then x else 1))

--- 8) indagar sore las funciones map y filter:
-- map: map f xs es una función la cual devuelve una lista con f aplicado a cada elemento de la lista xs.
-- filter: filter es una función, aplicada al predicado y a la lista, la cual devuelve una lista con los elementos que satisfacen al predicado.

-- map succ xs esquivale a una lista con el siguiente de cada número. map succ [1,-4,6,2,-8] es equivalente a [2,-3,7,3,-7]
-- filter esPositivo xs equivale a una lista con los elementos positivos de la lista xs. filter esPositivo [1,-4,6,2,-8] es equivalente a [1,6,2]

--- 9) 
-- a)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x*2 : (duplica xs) 
-- b)
duplica' :: [Int] -> [Int]
duplica' xs = map (*2) xs

--- 10)
-- a)
soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | mod x 2 == 0 = x:(soloPares xs)
                 | otherwise = soloPares xs
--b)
soloPares' :: [Int] -> [Int]
soloPares' xs = filter even xs
-- c)
multiplicaPares' :: [Int] -> Int
multiplicaPares' xs = productoria (filter even xs)

--- 11)
-- a)
sumarALista :: Num a => a -> [a] -> [a]
sumarALista _ [] = []
sumarALista n (x:xs) = (n+x) : (sumarALista n xs)

encabezar :: a -> [[a]] -> [[a]] 
encabezar _ [] = []
encabezar n (xs:ys) = (n:xs) : (encabezar n ys) 

mayoresA :: Ord a => a -> [a] -> [a]
mayoresA _ [] = []
mayoresA n (x:xs) | x > n = x : (mayoresA n xs) 
                  | otherwise = (mayoresA n xs)

-- b)
sumarALista' :: Num a => a -> [a] -> [a]
sumarALista' n xs = map (+n) xs

encabezar' :: a -> [[a]] -> [[a]]
encabezar' n xs = map (n: ) xs

mayoresA' :: Ord a => a -> [a] -> [a]
mayoresA' n xs = filter (n< ) xs

--- 12)
sacarStr :: [(Int,String)] -> [String]
sacarStr [] = []
sacarStr ((_,b):xs) = b : sacarStr xs

encuentra' :: Int -> [(Int,String)] -> String
encuentra' n xs = head (sacarStr (filter (\(a,_) -> (a == n)) xs))

--- 13)
-- a)
primIgualA :: Eq a => a -> [a] -> [a] 
primIgualA _ [] = [] 
primIgualA n (x:xs) | n == x = (x:(primIgualA n xs))
                    | otherwise = []
-- b)
primIgualA' :: Eq a => a -> [a] -> [a]
primIgualA' n xs = takeWhile (==n) xs

--- 14)
-- a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales (x:xs) | x == xs!!0 = (x:(primIguales xs))
                   | otherwise = [x]
-- b)
primIguales' :: Eq a => [a] -> [a]
primIguales' xs = primIgualA' (xs!!0) xs

--- 15)
--a)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) | x < minimo xs = x
              | otherwise = minimo xs
minimo _ = error ("No definido para la lista []") 

-- b)
minimo' :: (Ord a, Bounded a) => [a] -> a
minimo' [] = error "Error" 
minimo' [x] = x
minimo' (x:xs) | x < (minimo' xs) = x
               | otherwise = minimo' xs
