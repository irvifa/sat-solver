.PHONY: build run clean show next

build:
	ghc -o sudoku_solver sudoku_solver.hs

run:
	./sudoku_solver ${PUZZLE} ${MINISATPATH}

clean:
	rm sudoku_solver sudoku_solver.o sudoku_solver.hi answer.txt sudoku.out sudoku.cnf

show:
	cat answer.txt

next:
	./sudoku_solver -n ${MINISATPATH}
