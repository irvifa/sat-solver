import Data.List
import System.IO
import System.Process
import Control.Applicative
import Data.List.Split
import System.Environment
import System.Exit

readInt:: String -> Int
readInt = read

-- This function will generate the cartesean product of a list with itself
-- The param that being given here is list of a tuple of integer
cartesianProd (list,sizeOfBoard) = [(x,y,sizeOfBoard) | x <- list, y <- list]

-- Each of sudoku cell is being represented as triples where the first and second
-- entry specify the row and colum (zero indexed) and the third value is representing the 
-- label that being given the value is lie within the range of (1-N)
-- In the further assistance we will create a bijection function between the triples and
-- natural numbers that will serve as the boolean variable name
-- The param of this function, which is (i, j, k) will representing the row, column, and
-- The index that lies within the value of (1..N)
-- The function that being created here is given as follows:
-- the third value will be multiplied by the constant of NxN
-- the second value will be multiplied by the constant of N
-- the third value will be written as it is.
cellToVar (i,j,k,sizeOfBoard) = fromIntegral $ (k-1)*sizeOfBoard*sizeOfBoard + i*sizeOfBoard + j + 1

-- This function is serving as the inverse function of above ffunction, that is converting 
-- the natural number value that serve as the boolean variable back into the triples of rows, 
-- column, and index
varToCell (x,sizeOfBoard) = ((i `mod` (sizeOfBoard*sizeOfBoard)) `div` sizeOfBoard, i `mod` sizeOfBoard, (i `div` (sizeOfBoard*sizeOfBoard))+1)
	where i = (fromIntegral x)-1

-- This clause ensure that any given cell will be labeled exactly by one value.
-- This function will further check whether the cell wont be labeled by the same number
-- and then ensure that the cell is being labeled by at least one value
hasOnlyOne (i,j,sizeOfBoard) = atLeastOne : lessThan2
	where notBoth (c1,c2) = [- cellToVar (i,j,c1,sizeOfBoard), - cellToVar (i,j,c2,sizeOfBoard)]
              lessThan2  = map notBoth $ [(i,j) | (i,j,l) <- cartesianProd ([1..sizeOfBoard],sizeOfBoard), i /= j]
              atLeastOne = map cellToVar [(i,j,k,sizeOfBoard) | k <- [1..sizeOfBoard]]

-- This is a list of clauses that ensure each of the cell has exactly one value
validClause sizeOfBoard = foldr ((++).hasOnlyOne) [] $ cartesianProd ([0..sizeOfBoard-1],sizeOfBoard)

-- This is a general constraint of the group of each cell that formed the square
-- This will help listing all of the square groups of cells
sgConstraint sizeOfBoard= [quadrent i j | (i,j,l) <- cartesianProd ([0..round (sqrt (fromIntegral sizeOfBoard))-1],sizeOfBoard)]
	where quadrent x y = [(x*round (sqrt (fromIntegral sizeOfBoard))+i,y*round (sqrt (fromIntegral sizeOfBoard))+j) | (i,j,l) <- cartesianProd ([0..round (sqrt (fromIntegral sizeOfBoard))-1],sizeOfBoard)]

-- This is list of available rows
rows sizeOfBoard = [[(i,j) | i <- [0..sizeOfBoard-1]] | j <- [0..sizeOfBoard-1]]

-- This is list of available column
cols sizeOfBoard = [[(i,j) | j <- [0..sizeOfBoard-1]] | i <- [0..sizeOfBoard-1]]

-- This constraint will ensure that a group of cells contains at least one of all labels [1-9].
validGroup (group,sizeOfBoard) = foldr ((:).label) [] [1..sizeOfBoard]
	where label k = map cellToVar [(i,j,k,sizeOfBoard) | (i,j) <- group ]

-- This constraint will ensure that the labeling contraint is in a good condition
-- A label is "good" if it satisfies each of available constraints, that is every square, row.
-- and colum contains one of each label

createTuple(list,sizeOfBoard) = [(x,sizeOfBoard) | x <- list]
validLabel sizeOfBoard = foldr ((++).validGroup) [] tmp
	where tmp = createTuple((sgConstraint sizeOfBoard ++ rows sizeOfBoard ++ cols sizeOfBoard),sizeOfBoard)

-- Produce a formula for a set of sudoku constraints - filled in cells,
-- for which a model describes a sudoku solution.

-- This function will create a formula for a set of available constraints
-- This is the model for sudoku solution
sudokuForm (cells,sizeOfBoard) = validClause sizeOfBoard ++ validLabel sizeOfBoard ++ (map (\(i,j,k) -> consClause(i,j,k)) cells)
	where 
		consClause (i,j,k) = [cellToVar (i,j,k,sizeOfBoard)]

-- This fucntion will create a DIMACS formatted CNF from CNF formula
-- A CNF formula is being represented by list of list of integers (list of clauses)
-- Variable will be represented as positive integers, not the negative one
cnfToDIMACS cnf = header ++ (unlines $ map showClause cnf)
	where max = maximum $ map maximum cnf
              header = "p cnf " ++ (show max) ++ " " ++ (show $ length cnf) ++ "\n"
              showClause clause =
               (foldl1 (\a b -> a ++ " " ++ b)  (map show clause)) ++ " 0"

-- This function will call minisat on a given dimacs formula
-- The output of this function is a list of integer
-- The value is true if the list contain positive integer
-- otherwise it's false
minisatRunner (cnf,path) = do
	let dimacs = cnfToDIMACS cnf
	writeFile "sudoku.cnf" dimacs
	let cmd = path ++ " sudoku.cnf sudoku.out"
	system cmd
	vars <- preProcessOutput <$> readFile "sudoku.out"
	return vars

-- This function will preprocess the output file of the previous minisatRunner
-- The value will be [] if it's unsatisfiable
preProcessOutput f | head (lines f) /= "SAT" = []
                    | otherwise = vars
	where vars = delete 0 $ map readInt $ splitOn " " $ (lines f !! 1)

-- This will using the inverse function provided in the above that is mapping back
-- the number into the value of triplets
modelToMatrix ([],_) = []
modelToMatrix (model,sizeOfBoard) = [[lookup i j | j <- [0..sizeOfBoard-1]] | i <- [0..sizeOfBoard-1]]
	where
        cells = map (\x -> varToCell(x,sizeOfBoard)) $ filter ((<) 0) model
        lookup i j = label $ head $ filter (\(a,b,_) -> a == i && b == j) cells
        label (_,_,c) = c

-- This function will get all of the available constraints from sudoku matrix.
getConstraints (matrix,sizeOfBoard) = filter (\(_,_,a) -> a > 0) cells
	where flat = foldl1 (++) matrix
	      cells = zip3 [i `div` sizeOfBoard | i <- [0..]] (cycle [0..sizeOfBoard-1]) flat

-- Solve a sudoku matrix.
sudokuSolve (matrix,path) = do
	let sizeOfBoard = length matrix
	let mat = getConstraints (matrix, sizeOfBoard)
	let cnf = sudokuForm (mat,sizeOfBoard)
	minisatout <- minisatRunner (cnf,path)
	return $ modelToMatrix (minisatout,sizeOfBoard)

-- Return a string as a solution of given sudoku matrix
showMatrix [] = "no solution"
showMatrix grid = unlines $ map (foldr ((++).show) []) grid

-- Get each of 9 char from the list and then trying to find the solution of the given problem
-- The solution of this problem will be saved in the file named answer.txt
-- solveList ([],_) = do return 0
solveList (list,sizeOfBoard,path) = do
	let (puzzle, rest) = splitAt sizeOfBoard list
	-- let mat = stringToMatrix $ unlines puzzle
	solution <- sudokuSolve (list,path)
	let ans = showMatrix solution
	writeFile "answer.txt" ans 


-- Get minisat path
getPath args = if length args==2 then last args else "minisat"

-- This method will solve sudoku that being given as the first param
-- File name is the sudoku puzzle that being given
wrapper args = do
			let path = getPath args
			let fileName = head args
			contents <- splitOneOf "\n " <$> readFile fileName
			let tmp = init contents
			let pruned = map readInt tmp
			let sizeOfBoard = round (sqrt (fromIntegral $ length pruned)) 
			let matrix = chunksOf sizeOfBoard pruned
			solveList (matrix,sizeOfBoard,path)

-- Generate more solution
genMore = do
		let cnfFile = "sudoku.cnf"
		let solFile = "sudoku.out"
		let path = "./../minisat/core/minisat"
		let fileName = "board.txt"
		
		board <- splitOneOf "\n " <$> readFile fileName
		let tmp = init board
		let pruned = map readInt tmp
		let sizeOfBoard = round (sqrt (fromIntegral $ length pruned)) 

		
		contents <- splitOneOf "\n " <$> readFile solFile
		let pruned = map readInt $ init $ tail contents
		let tmp =  intercalate " " $ map show $ map ( * (-1) ) pruned
		let res = tmp ++ "\n"
		appendFile cnfFile res


		cnf <- splitOneOf "\n" <$> readFile "sudoku.cnf"
		let fiCnf = splitOneOf " " $ head cnf
		let lastCnf = tail cnf
		let fiRow = (readInt $ last fiCnf) + 1
		let restRow = intercalate " " (init fiCnf ++ [show fiRow])
		let newCnf = intercalate "\n" $ init $ restRow:lastCnf 
		writeFile "newCnf.txt" newCnf
		
		let mv = "mv newCnf.txt sudoku.cnf"
		system mv

		let cmd = path ++ " sudoku.cnf sudoku.out"
		system cmd

		minisatout <- preProcessOutput <$> readFile "sudoku.out"
		let ans = showMatrix $ modelToMatrix (minisatout,sizeOfBoard)
		writeFile "answer.txt" ans 

-- Several flag and it's usage 
usage   = putStrLn "Usage ./sudoku_solve [input-file] [minisat-path]"
version = putStrLn "Version 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["-n"] = genMore >> exit
parse []     = usage   >> exit
parse args     =  wrapper args >> exit

guide  = unlines . reverse . lines

-- Main function
main = getArgs >>= parse >>= putStr . guide