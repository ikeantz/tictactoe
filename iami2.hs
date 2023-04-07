import PickRandom 
import Control.Concurrent -- .Chan
import Control.Monad --(forever)
import Data.Maybe --(isNothing)
import Data.Char (toUpper)

-- (1):1 Types for Board, a Move, and an Outcome.

type Board = [[Char]]
-- Generate a board, using a character string as a placeholder
type Move = (Char, Int)
-- A move is defined by a Character and Int for example: place X at A3
data Outcome = Win Char | Draw | Ongoing deriving (Eq, Show)
-- The outcome is Win (X or O), Draw or Ongoing game.

-- (1):2 Function to print the game board in a human readable format

-- Implementing the print board function
printBoard :: Board -> IO ()
printBoard board = do
    -- The header contains A B C equally spaced out
  putStrLn "  A B C"
  mapM_ printRow (zip [1..] board)
  where
    -- Now we're going to describe what can go in each Cell.
    printRow (i, row) = putStrLn $ show i ++ " " ++ unwords (map showCell row)
    -- Each empty cell contains * 
    showCell ' ' = "*"
    showCell c = [c]

-- (1):3 Function to check whether the move is legal in the board.

-- Defining a valid move
makeMove :: Board -> Move -> Maybe Board
makeMove board (col, row)
-- Check to see if the Cells are empty
  | row < 1 || row > 3 || col < 'A' || col > 'C' = Nothing
  | board !! (row - 1) !! (colToInt col - 1) == ' ' = Just $ setBoard board (colToInt col - 1, row - 1) player
  | otherwise = Nothing
  where
    -- Check to see if placement is out of bound and X or O is placed in the individual Cell
    player = if even (length (filter (/= ' ') (concat board))) then 'X' else 'O'
    colToInt c = fromEnum c - fromEnum 'A' + 1
    setBoard b (x, y) c = take y b ++ [(take x (b !! y) ++ [c] ++ drop (x+1) (b !! y))] ++ drop (y+1) b

-- (1):4 Get the outcome of the game (get the winner)

checkOutcome :: Board -> Maybe Outcome
checkOutcome board
-- If neither X or O have a valid combination is ends in a draw
  | any isWinningLine allLines = Just $ Win player
  | any (== ' ') (concat board) = Just Ongoing
  | otherwise = Just Draw
  -- Checks winning line
  where
    player = if even (length (filter (/= ' ') (concat board))) then 'O' else 'X'
    allLines = rows ++ cols ++ diags
    rows = board
    cols = transpose board
    diags = [[board !! i !! i | i <- [0..2]], [board !! i !! (2 - i) | i <- [0..2]]]
    isWinningLine line = all (== 'X') line || all (== 'O') line

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = (map head x) : transpose (map tail x)

-- (2):5 Define a human player in the game.

type InChan = Chan (Board, Int)
type OutChan = Chan Move

-- Defining two different players
data Player = HumanPlayer Char InChan OutChan | BotPlayer Char InChan OutChan

humanPlayer :: Player -> IO ()
humanPlayer (HumanPlayer token inChan outChan) = forever $ do
    (board, moveCount) <- readChan inChan
    -- Print onto the board
    printBoard board
    -- Display below table
    putStrLn $ "It is Player " ++ [token] ++ "'s turn. Please enter your move (e.g., A3):"
    moveStr <- getLine
    let move = (head moveStr, read (tail moveStr) :: Int)
    -- Checks for illegal move
    -- Takes a Player and runs the game loop for the user.
    case makeMove board move of
        Nothing -> do
            putStrLn "illegal move. Please try again."
            writeChan inChan (board, moveCount)
        Just newBoard -> writeChan outChan move

-- (2):6 Define the botPlayer (Computer Interactions)


botPlayer :: Player -> IO ()
botPlayer (BotPlayer token inChan outChan) = forever $ do
    (board, moveCount) <- readChan inChan
    legalMoves <- generateLegalMoves board
    -- Randomly generate a point on the Cell to place O on
    -- Takes a pPlayer and runs the game loop for the bot.
    move <- pickRandom legalMoves
    writeChan outChan move
  where
    -- Only makes legal moves by selecting randomly
    generateLegalMoves :: Board -> IO [Move]
    generateLegalMoves board = do
      let allMoves = [(col, row) | col <- ['A', 'B', 'C'], row <- [1, 2, 3]]
      return $ filter (\move -> isJust $ makeMove board move) allMoves

-- (2):7 Game manaiing process. Keep track of boards position

gameManager :: Player -> Player -> IO ()
-- p1 = human player so they place first and p2 = bot so they play second
gameManager p1@(HumanPlayer t1 inChan1 outChan1) p2@(BotPlayer t2 inChan2 outChan2) = gameLoop emptyBoard 0
  where
    -- Create a 3x3 board
    emptyBoard = replicate 3 (replicate 3 ' ')

    gameLoop :: Board -> Int -> IO ()
    gameLoop board moveCount = do
        case checkOutcome board of
            -- Prints if player X or O wins
            Just (Win winner) -> do
                printBoard board
                putStrLn $ "Player " ++ [winner] ++ " wins!"
                -- For draw
            Just Draw -> do
                printBoard board
                putStrLn "It's a draw!"
                -- Update sthe board for the next player's turn
            Just Ongoing -> do
                let currentPlayer = if even moveCount then p1 else p2
                let currInChan = case currentPlayer of
                                  HumanPlayer _ inChan _ -> inChan
                                  BotPlayer _ inChan _ -> inChan
                let currOutChan = case currentPlayer of
                                HumanPlayer _ _ outChan -> outChan
                                BotPlayer _ _ outChan -> outChan
                writeChan currInChan (board, moveCount)
                newMove <- readChan currOutChan
                let newBoard = fromJust $ makeMove board newMove
                gameLoop newBoard (moveCount + 1)
            _ -> return ()

-- (2):8 Game start module or 'main method to start the game with necessary channels

-- Main sets up the channels and all the players. The players process in a seperate threads using module forkIO.
-- This means the game will only run until there's a winner.

startGame :: IO ()
startGame = do
    inChan1 <- newChan
    outChan1 <- newChan
    inChan2 <- newChan
    outChan2 <- newChan
    let player1 = HumanPlayer 'X' inChan1 outChan1
    let player2 = BotPlayer 'O' inChan2 outChan2
    forkIO $ humanPlayer player1
    forkIO $ botPlayer player2
    gameManager player1 player2

-- Printing the gameBoar check
--emptyBoard :: Board
--emptyBoard = replicate 3 (replicate 3 Empty)

--main :: IO ()
--main = printBoard emptyBoard