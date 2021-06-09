import Data.Char

--2
bmi' :: Float -> Float -> String
bmi' p a 
  | imc >= 30 = "ACIMA"
  | imc <= 18.5 = "ABAIXO"
  | otherwise = "NORMAL"

  where imc = p/a^2

--3
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
 let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11-expr

main :: IO()
main = do
 let cpf = "22233344405"
     digits = (map digitToInt cpf)
     result = cpfValid digits
 putStrLn (show result)

--4
andTable :: [(Bool, Bool, Bool)]
andTable = [(x, y,  x && y) | x <- [True, False], y <- [True, False]]

--[(True,True,True),(True,False,False),(False,True,False),(False,False,False)]