import Data.Char

-- Función para convertir un carácter en su valor numérico, teniendo en cuenta el caso especial de 'X'
charToDigit :: Char -> Int
charToDigit 'X' = 10
charToDigit c = digitToInt c

-- Función para verificar un código ISBN
isValidISBN :: String -> Bool
isValidISBN isbn =
    let cleanedISBN = map charToDigit (filter (\c -> isDigit c || c == 'X') isbn)
        weightedDigits = zipWith (*) cleanedISBN [10, 9 .. 1]
        checksum = sum weightedDigits `mod` 11
    in checksum == 0

-- Función principal para la entrada y salida
main :: IO ()
main = do
    putStrLn "Ingrese el código ISBN:"
    input <- getLine
    let result = isValidISBN input
    putStrLn ("Resultado: " ++ show result)

