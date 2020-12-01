module DayOne (solutions) where

solutions :: IO ()
solutions =
  do
    linesAsNumbers "./data/01.data"
    >>= ( \ns ->
                 print (solveOne ns)
              >> print (solveTwo ns)
        )

linesAsNumbers :: String -> IO [Integer]
linesAsNumbers filepath =
  map read . lines <$> readFile filepath

solveOne :: [Integer] -> Integer
solveOne numbers =
  head [a * b | a <- numbers, b <- numbers, a + b == 2020]

solveTwo :: [Integer] -> Integer
solveTwo numbers =
  head [a * b * c | a <- numbers, b <- numbers, c <- numbers, a + b + c == 2020]
