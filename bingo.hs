-- BRUNO DE OLIVEIRA JUCÁ - 201965013A
-- RÔMULO CHRISPIM DE MELLO - 201935038

import System.Random
import Data.List

-- Gera uma lista de numeros aleatorios entre min e max que nao se repetem e que nao estao inicialmente em list
generateRandomList :: Int -> [Int] -> Int -> Int -> IO [Int]
generateRandomList num list min max = do
  if length list == num
    then return list
  else do
    randnum <- randomRIO(min, max)

    if elem randnum list
      then generateRandomList num list min max
    else do
      let newlist = list ++ [randnum]
      generateRandomList num newlist min max

-- Gera um numero aleatorio entre min e max que nao esteja em list
generateRandomNum :: [Int] -> Int -> Int -> IO Int
generateRandomNum list min max = do
  randnum <- randomRIO(min, max)

  if elem randnum list
    then generateRandomNum list min max
  else do
    return randnum
     

-- Gera lista de 25 numeros representando as cartelas de acordo com as regras especificadas
generateCard :: IO [Int]
generateCard = do

  numbers1to15 <- generateRandomList 5 [] 1 15
  numbers16to30 <- generateRandomList 5 [] 16 30
  numbers31to45 <- generateRandomList 5 [] 31 45
  numbers46to60 <- generateRandomList 5 [] 46 60
  numbers61to75 <- generateRandomList 5 [] 61 75

  let numbers = sort ( numbers1to15 ++ numbers16to30 ++ numbers31to45 ++ numbers46to60 ++ numbers61to75 )
  
  return numbers

-- Checagem se alguem ganhou. Divida entre o tipo de cartela para linhas ou colunas.
checkBingo :: [Int] -> [Int] -> String -> Bool
checkBingo calledNumbers card "l"
  | length (intersect firstLine calledNumbers) == 5 = True
  | length (intersect secondLine calledNumbers) == 5 = True
  | length (intersect thirdLine calledNumbers) == 5 = True
  | length (intersect fourthLine calledNumbers) == 5 = True
  | length (intersect fifthLine calledNumbers) == 5 = True
  | otherwise = False
  where
    firstLine = take 5 card
    secondLine = take 5 $ drop 5 card
    thirdLine = take 5 $ drop 10 card
    fourthLine = take 5 $ drop 15 card
    fifthLine = drop 20 card

checkBingo calledNumbers card "c"
  | length (intersect firstColumn calledNumbers) == 5 = True
  | length (intersect secondColumn calledNumbers) == 5 = True
  | length (intersect thirdColumn calledNumbers) == 5 = True
  | length (intersect fourthColumn calledNumbers) == 5 = True
  | length (intersect fifthColumn calledNumbers) == 5 = True
  | otherwise = False
  where
    firstColumn = [x | (x, i) <- zip card [0..], (i `mod` 5) == 0]
    secondColumn = [x | (x, i) <- zip card [0..], (i `mod` 5) == 1]
    thirdColumn = [x | (x, i) <- zip card [0..], (i `mod` 5) == 2]
    fourthColumn = [x | (x, i) <- zip card [0..], (i `mod` 5) == 3]
    fifthColumn = [x | (x, i) <- zip card [0..], (i `mod` 5) == 4]

-- Rodadas do bingo
playRound :: [Int] -> [(String, String, [Int])] -> IO ()
playRound calledNumbers players = do
  -- Sorteia um numero
  if length calledNumbers == 75
    then putStrLn "Todos os numeros foram sorteados"
    else do
      number <- generateRandomNum calledNumbers 1 75
      let updatedCalledNumbers = calledNumbers ++ [number]
      putStrLn $ "======= Valor Sorteado: " ++ show number ++ " ======="

      -- Imprime as cartelas
      putStrLn "Cartelas:"
      mapM_ (\(name, cardType, card) -> do
               putStrLn $ "========== " ++ name ++ ": " ++ (if cardType == "l" then "linha" else "coluna") ++ " =========="
               mapM_ (\n -> putStr $ show n ++ (if n<10 then " " else "") ++" [" ++ (if (elem n updatedCalledNumbers) then "X" else " ") ++ "] ") $ take 5 card
               putStrLn ""
               mapM_ (\n -> putStr $ show n ++ " [" ++ (if (elem n updatedCalledNumbers) then "X" else " ") ++ "] ") $ take 5 $ drop 5 card
               putStrLn ""
               mapM_ (\n -> putStr $ show n ++ " [" ++ (if (elem n updatedCalledNumbers) then "X" else " ") ++ "] ") $ take 5 $ drop 10 card
               putStrLn ""
               mapM_ (\n -> putStr $ show n ++ " [" ++ (if (elem n updatedCalledNumbers) then "X" else " ") ++ "] ") $ take 5 $ drop 15 card
               putStrLn ""
               mapM_ (\n -> putStr $ show n ++ " [" ++ (if (elem n updatedCalledNumbers) then "X" else " ") ++ "] ") $ drop 20 card
               putStrLn ""
               putStrLn "") players

      -- Verifica se alguem ganhou e aguarda por input para continuar
      let winners = filter (/= "") $ map(\(name, cardType, card) -> if (checkBingo updatedCalledNumbers card cardType) then name else "" ) players
      if (length winners) /= 0
        then mapM_ (\winner -> do
          putStrLn $ "BINGO!\nGanhador: " ++  winner) winners
        else do
          putStrLn "Aperte enter para realizar uma rodada"
          _ <- getLine
          playRound updatedCalledNumbers players


-- Funcao principal para selecionar o numero de jogadores, receber seus dados
-- e gerar suas cartelas
main :: IO ()
main = do
  -- Quantidade de jogadores
  putStrLn "Informe a quantidade de jogadores: "
  players <- getLine
  let numPlayers = read players :: Int

  -- Nome dos jogadores
  names <- sequence [putStrLn ("Qual o nome do jogador " ++ show i ++ "? ") >> getLine | i <- [1..numPlayers]]

  -- Tipo de cartela dos jogadores
  cardTypes <- sequence [putStrLn ("Digite o tipo de cartela de " ++ (names !! (i-1)) ++ " - (l) para linha e (c) para coluna: ") >> getLine | i <- [1..numPlayers] ]
  putStrLn ""

  -- Gera as cartelas dos jogadores
  cards <- sequence [generateCard | _ <- [1..numPlayers]]

  let calledNumbers = []

  -- Inicia rodadas
  playRound calledNumbers (zip3 names cardTypes cards)
  