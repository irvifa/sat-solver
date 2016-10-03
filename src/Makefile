.PHONY: build run clean show

build:
	ghc -o sudoku_solver sudoku_solver.hs

run:
	./sudoku_solver ${PUZZLE}

clean:
	rm sudoku_solver sudoku_solver.o sudoku_solver.hi answer.txt sudoku.out sudoku.cnf

show:
	cat answer.txt