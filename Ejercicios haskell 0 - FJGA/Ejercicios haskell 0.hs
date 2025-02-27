--Francisco Javier Gordillo Aguilar - S22120154

-- Primera funcion
sumarLista :: [Int] -> Int
sumarLista = sum

-- Segunda funcion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Tercera funcion
pares :: Int -> [Int]
pares x = filter (\n -> n `mod` 2 == 0) [1..x]

-- Cuarta funcion
longitudCadena :: String -> Int
longitudCadena x = length x 

--Quinta funcion
reversoLista :: [a] -> [a]
reversoLista x = reverse x

--Sexta funcion
duplicarElementos :: [Int] -> [Int]
duplicarElementos [] = []
duplicarElementos (x:xs) = x : x : duplicarElementos xs

--Septima funcion
filtrarPares :: [Int] -> [Int]
filtrarPares x = filter (\n -> n `mod` 2 /= 0) x

--Octava funcion
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--Novena funcion
divisores :: Int -> [Int]
divisores x = filter (\n -> x `mod` n == 0) [1..x]

--decima funcion
esPalindromo :: String -> Bool
esPalindromo x = x == reverse x

main :: IO ()
main = do
    print(sumarLista [1,2,3,4,5])
    print (factorial 5)
    print(pares 20)
    print (longitudCadena "HOLA ADIOS")
    print(reversoLista ["Hola","adios","yaVamonos"])
    print(duplicarElementos [1,2])
    print(filtrarPares [1,2,3,4,5,6,7,8,9,10])
    print(fibonacci 9)
    print(divisores 10)
    print(esPalindromo "Holaalo")
    print(esPalindromo "HolaaloH")