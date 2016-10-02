#!/bin/bash

compile() {
	ghc -o sudoku_solver sudoku_solver.hs
}

run() {
	./sudoku_solver $1
}

compile
run
