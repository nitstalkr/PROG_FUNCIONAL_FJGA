aTexto :: Int -> String
numeros :: [String]
numeros = ["ZERO", "ONE", "TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE"]
decenas = ["ZERO", "ONE", "TWEN", "THIR", "FOUR", "FIF", "SIX", "SEVEN", "EIGHT", "NINE"]
aTexto x
    |x>=1 && x<=9 = numeros !! x
    |x==10 = "TEN"
    |x==11 = "ELEVEN"
    |x==12 = "TWELEVE"
    |x>=13 && x<=19 = decenas !! mod x 10 ++ "TEEN"
    |otherwise = decenas !! div x 10 ++ "TY " ++ numeros !! mod x 10



kataFizzBuzz :: Int -> String
kataFizzBuzz x
    | x `mod` 3==0 && x `mod` 5==0 = "fizzBuzz!"
    | x `mod` 3==0 = "fizz!"
    | x `mod` 5==0 = "buzz!"
    | otherwise = aTexto x

test :: Int -> IO ()
test actual
    | actual == 101 = return () 
    | otherwise = do
        let resultado = kataFizzBuzz actual
        print (show actual ++ " " ++ resultado)
        test (actual+1)  


main :: IO ()
main = do
  test 1
  