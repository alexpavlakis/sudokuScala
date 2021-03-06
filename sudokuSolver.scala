// This program solves sudoku puzzles structured as ListBuffers, 
// with 0 for unknown elements with a fast backtracking algorithm

import scala.collection.mutable.ListBuffer

object Sudoku {

	// The indices of peers for each element (81x20)
	val peers = List( List(1,2,3,4,5,6,7,8,9,10,11,18,19,20,27,36,45,54,63,72),
			          List(0,2,3,4,5,6,7,8,9,10,11,18,19,20,28,37,46,55,64,73),
			          List(0,1,3,4,5,6,7,8,9,10,11,18,19,20,29,38,47,56,65,74),
			          List(0,1,2,4,5,6,7,8,12,13,14,21,22,23,30,39,48,57,66,75),
			          List(0,1,2,3,5,6,7,8,12,13,14,21,22,23,31,40,49,58,67,76),
			          List(0,1,2,3,4,6,7,8,12,13,14,21,22,23,32,41,50,59,68,77),
			          List(0,1,2,3,4,5,7,8,15,16,17,24,25,26,33,42,51,60,69,78),
			          List(0,1,2,3,4,5,6,8,15,16,17,24,25,26,34,43,52,61,70,79),
			          List(0,1,2,3,4,5,6,7,15,16,17,24,25,26,35,44,53,62,71,80),
			          List(0,1,2,10,11,12,13,14,15,16,17,18,19,20,27,36,45,54,63,72),
			          List(0,1,2,9,11,12,13,14,15,16,17,18,19,20,28,37,46,55,64,73),
			          List(0,1,2,9,10,12,13,14,15,16,17,18,19,20,29,38,47,56,65,74),
			          List(3,4,5,9,10,11,13,14,15,16,17,21,22,23,30,39,48,57,66,75),
			          List(3,4,5,9,10,11,12,14,15,16,17,21,22,23,31,40,49,58,67,76),
			          List(3,4,5,9,10,11,12,13,15,16,17,21,22,23,32,41,50,59,68,77),
			          List(6,7,8,9,10,11,12,13,14,16,17,24,25,26,33,42,51,60,69,78),
			          List(6,7,8,9,10,11,12,13,14,15,17,24,25,26,34,43,52,61,70,79),
			          List(6,7,8,9,10,11,12,13,14,15,16,24,25,26,35,44,53,62,71,80),
			          List(0,1,2,9,10,11,19,20,21,22,23,24,25,26,27,36,45,54,63,72),
			          List(0,1,2,9,10,11,18,20,21,22,23,24,25,26,28,37,46,55,64,73),
			          List(0,1,2,9,10,11,18,19,21,22,23,24,25,26,29,38,47,56,65,74),
			          List(3,4,5,12,13,14,18,19,20,22,23,24,25,26,30,39,48,57,66,75),
			          List(3,4,5,12,13,14,18,19,20,21,23,24,25,26,31,40,49,58,67,76),
			          List(3,4,5,12,13,14,18,19,20,21,22,24,25,26,32,41,50,59,68,77),
			          List(6,7,8,15,16,17,18,19,20,21,22,23,25,26,33,42,51,60,69,78),
			          List(6,7,8,15,16,17,18,19,20,21,22,23,24,26,34,43,52,61,70,79),
			          List(6,7,8,15,16,17,18,19,20,21,22,23,24,25,35,44,53,62,71,80),
			          List(0,9,18,28,29,30,31,32,33,34,35,36,37,38,45,46,47,54,63,72),
			          List(1,10,19,27,29,30,31,32,33,34,35,36,37,38,45,46,47,55,64,73),
			          List(2,11,20,27,28,30,31,32,33,34,35,36,37,38,45,46,47,56,65,74),
			          List(3,12,21,27,28,29,31,32,33,34,35,39,40,41,48,49,50,57,66,75),
			          List(4,13,22,27,28,29,30,32,33,34,35,39,40,41,48,49,50,58,67,76),
			          List(5,14,23,27,28,29,30,31,33,34,35,39,40,41,48,49,50,59,68,77),
			          List(6,15,24,27,28,29,30,31,32,34,35,42,43,44,51,52,53,60,69,78),
			          List(7,16,25,27,28,29,30,31,32,33,35,42,43,44,51,52,53,61,70,79),
			          List(8,17,26,27,28,29,30,31,32,33,34,42,43,44,51,52,53,62,71,80),
			          List(0,9,18,27,28,29,37,38,39,40,41,42,43,44,45,46,47,54,63,72),
			          List(1,10,19,27,28,29,36,38,39,40,41,42,43,44,45,46,47,55,64,73),
			          List(2,11,20,27,28,29,36,37,39,40,41,42,43,44,45,46,47,56,65,74),
			          List(3,12,21,30,31,32,36,37,38,40,41,42,43,44,48,49,50,57,66,75),
			          List(4,13,22,30,31,32,36,37,38,39,41,42,43,44,48,49,50,58,67,76),
			          List(5,14,23,30,31,32,36,37,38,39,40,42,43,44,48,49,50,59,68,77),
			          List(6,15,24,33,34,35,36,37,38,39,40,41,43,44,51,52,53,60,69,78),
			          List(7,16,25,33,34,35,36,37,38,39,40,41,42,44,51,52,53,61,70,79),
			          List(8,17,26,33,34,35,36,37,38,39,40,41,42,43,51,52,53,62,71,80),
			          List(0,9,18,27,28,29,36,37,38,46,47,48,49,50,51,52,53,54,63,72),
			          List(1,10,19,27,28,29,36,37,38,45,47,48,49,50,51,52,53,55,64,73),
			          List(2,11,20,27,28,29,36,37,38,45,46,48,49,50,51,52,53,56,65,74),
			          List(3,12,21,30,31,32,39,40,41,45,46,47,49,50,51,52,53,57,66,75),
			          List(4,13,22,30,31,32,39,40,41,45,46,47,48,50,51,52,53,58,67,76),
			          List(5,14,23,30,31,32,39,40,41,45,46,47,48,49,51,52,53,59,68,77),
			          List(6,15,24,33,34,35,42,43,44,45,46,47,48,49,50,52,53,60,69,78),
			          List(7,16,25,33,34,35,42,43,44,45,46,47,48,49,50,51,53,61,70,79),
			          List(8,17,26,33,34,35,42,43,44,45,46,47,48,49,50,51,52,62,71,80),
			          List(0,9,18,27,36,45,55,56,57,58,59,60,61,62,63,64,65,72,73,74),
			          List(1,10,19,28,37,46,54,56,57,58,59,60,61,62,63,64,65,72,73,74),
			          List(2,11,20,29,38,47,54,55,57,58,59,60,61,62,63,64,65,72,73,74),
			          List(3,12,21,30,39,48,54,55,56,58,59,60,61,62,66,67,68,75,76,77),
			          List(4,13,22,31,40,49,54,55,56,57,59,60,61,62,66,67,68,75,76,77),
			          List(5,14,23,32,41,50,54,55,56,57,58,60,61,62,66,67,68,75,76,77),
			          List(6,15,24,33,42,51,54,55,56,57,58,59,61,62,69,70,71,78,79,80),
			          List(7,16,25,34,43,52,54,55,56,57,58,59,60,62,69,70,71,78,79,80),
			          List(8,17,26,35,44,53,54,55,56,57,58,59,60,61,69,70,71,78,79,80),
			          List(0,9,18,27,36,45,54,55,56,64,65,66,67,68,69,70,71,72,73,74),
			          List(1,10,19,28,37,46,54,55,56,63,65,66,67,68,69,70,71,72,73,74),
			          List(2,11,20,29,38,47,54,55,56,63,64,66,67,68,69,70,71,72,73,74),
			          List(3,12,21,30,39,48,57,58,59,63,64,65,67,68,69,70,71,75,76,77),
			          List(4,13,22,31,40,49,57,58,59,63,64,65,66,68,69,70,71,75,76,77),
			          List(5,14,23,32,41,50,57,58,59,63,64,65,66,67,69,70,71,75,76,77),
			          List(6,15,24,33,42,51,60,61,62,63,64,65,66,67,68,70,71,78,79,80),
			          List(7,16,25,34,43,52,60,61,62,63,64,65,66,67,68,69,71,78,79,80),
			          List(8,17,26,35,44,53,60,61,62,63,64,65,66,67,68,69,70,78,79,80),
			          List(0,9,18,27,36,45,54,55,56,63,64,65,73,74,75,76,77,78,79,80),
			          List(1,10,19,28,37,46,54,55,56,63,64,65,72,74,75,76,77,78,79,80),
			          List(2,11,20,29,38,47,54,55,56,63,64,65,72,73,75,76,77,78,79,80),
			          List(3,12,21,30,39,48,57,58,59,66,67,68,72,73,74,76,77,78,79,80),
			          List(4,13,22,31,40,49,57,58,59,66,67,68,72,73,74,75,77,78,79,80),
			          List(5,14,23,32,41,50,57,58,59,66,67,68,72,73,74,75,76,78,79,80),
			          List(6,15,24,33,42,51,60,61,62,69,70,71,72,73,74,75,76,77,79,80),
			          List(7,16,25,34,43,52,60,61,62,69,70,71,72,73,74,75,76,77,78,80),
			          List(8,17,26,35,44,53,60,61,62,69,70,71,72,73,74,75,76,77,78,79) )



	// Function to print sudoku array in somewhat readable fashion
	def printSudoku (sudoku:ListBuffer[Int]) = {
		val rowBlock = "+ - - - + - - - + - - - +"
		var v = sudoku
		for(r <- 0.to(12)) {
			if(List(0, 4, 8, 12).contains(r)) print(rowBlock + "\n")
			else {
				for(c <- 0.to(12)) {
					if(List(0, 4, 8).contains(c)) print("| ")
					else if(c == 12) print("| \n")
					else {
						if(v(0) > 0) print(v(0) + " ")
					    else print("  ")
						v = v.drop(1)
					}
				}
			}
		}
	}

	// Function to get candidates for element
	def getCandidates(sudoku:ListBuffer[Int], peers:List[List[Int]]): ListBuffer[ListBuffer[Int]] = {
		var out = ListBuffer.fill(81)(ListBuffer.fill(1)(0))
		for(i <- 0.to(80)) {
			if(sudoku(i) > 0) {
				out(i) = ListBuffer(sudoku(i))
			} else {
				var options = ListBuffer(1,2,3,4,5,6,7,8,9)
				for(j <- 0.to(19)) {
					if(sudoku(peers(i)(j)) > 0 && options.contains(sudoku(peers(i)(j)))) {
						options -= sudoku(peers(i)(j))
					}
				}
				out(i) = options
			}
		}
		return out
	}


	// Function to get which elements are empty
	def getEmpties(sudoku:ListBuffer[Int]): ListBuffer[Int] = {
		var out = ListBuffer[Int]()
		for(i <- 0.to(80)) {
			if(sudoku(i) == 0) out += i
		}
		return out
	}

	// Function to get the number of candidates for each empty element
	def getLengths(empties:ListBuffer[Int], candidates:ListBuffer[ListBuffer[Int]]): ListBuffer[Int] = {
		var out = ListBuffer.fill(empties.length)(0)
		for(i <- 0.to(empties.length-1)) {
			out(i) = candidates(empties(i)).length
		}
		return out
	}


	// Function to sort empties by their number of candidates (ascending)
	def sortEmpties(empties:ListBuffer[Int], lengths:ListBuffer[Int]): ListBuffer[Int] = {
		val sortedEmpties = (empties zip lengths).sortBy(_._2).unzip._1
		return sortedEmpties
	}


	// Function to solve a puzzle by backtracking
def solveBacktracking(sudoku:ListBuffer[Int], empties:ListBuffer[Int], candidates:ListBuffer[ListBuffer[Int]]): Boolean = {
	
	if(empties.length == 0) return true 

	var index = empties(0)
	var clen = candidates(index).length 
	if(clen > 1) {
		var i = 0
		while(i < empties.length && clen > 1) {
			if(candidates(empties(i)).length < clen) {
				clen = candidates(empties(i)).length
				index = empties(i)
			}
			i += 1
		}
	}

	var options = candidates(index)
	for(o <- options) {
		var to_update = ListBuffer[Int]()
		var legal = true
		var j = 0
		while(j < 20 && legal) {
			var p = peers(index)(j)
			if(sudoku(p) == 0) {
				if(candidates(p).contains(o)) {
					if(candidates(p).length == 1) {
						legal = false
					}
					if(legal) {
						candidates(p) -= o
						to_update += p
					}
				}
			}
			j += 1
		} 

		if(legal) {
			sudoku(index) = o
			empties -= index
			if(solveBacktracking(sudoku, empties, candidates)) return true
			sudoku(index) = 0
			empties += index 
		}

		for(p <- to_update) {
			candidates(p) += o
		}
	}
	return false
}

	// Full sudoku solver
	def solveSudoku(sudoku:ListBuffer[Int]): ListBuffer[Int] = { 
		var empties = getEmpties(sudoku)
		var candidates = getCandidates(sudoku, peers)
		var lengths = getLengths(empties, candidates)
		empties = sortEmpties(empties, lengths)
		solveBacktracking(sudoku, empties, candidates)
		return sudoku;
	}

	// Some sample puzzles
	var easy_sudoku = ListBuffer(2,4,0,0,8,0,5,0,0,1,0,6,0,0,5,0,2,0,0,8,5,2,7,0,0,0,0,0,0,0,5,0,2,0,7,0,0,0,2,0,0,0,1,0,0,0,1,0,3,0,4,0,0,0,0,0,0,0,5,7,4,6,0,0,2,0,9,0,0,7,0,8,0,0,4,0,2,0,0,1,9)
	var hard_sudoku = ListBuffer(8,0,0,0,0,0,2,0,0,7,0,0,0,8,0,0,5,9,0,0,0,9,0,2,0,0,6,5,0,0,0,7,0,0,1,0,0,0,4,0,0,8,0,0,0,0,8,0,0,0,6,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,1,0,0,3,0,3,7,0,0,6,0,0,0,0)
	var evil_sudoku = ListBuffer(0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,0,3,0,0,2,3,0,0,4,0,0,0,0,1,8,0,0,0,0,5,0,6,0,0,7,0,8,0,0,0,0,0,0,0,9,0,0,0,0,0,8,5,0,0,0,0,0,9,0,0,0,4,0,5,0,0,4,7,0,0,0,6,0,0,0)

	// Time a Function
	def time[R](block: => R): R = {
	    val t0 = System.nanoTime()
	    val result = block    
	    val t1 = System.nanoTime()
	    println("Elapsed time: " + (t1 - t0)/1e+9 + " seconds")
	    return result
	}

	// Execute
	def main(args:Array[String]) : Unit = {
		print("easy sudoku\n")
		printSudoku(easy_sudoku)
		time {
			easy_sudoku = solveSudoku(easy_sudoku)
		}
		printSudoku(easy_sudoku)
		print("\n")

		print("hard sudoku\n")
		printSudoku(hard_sudoku)
		time {
			hard_sudoku = solveSudoku(hard_sudoku)
		}
		printSudoku(hard_sudoku)
		print("\n")

		print("evil sudoku\n")
		printSudoku(evil_sudoku)
		time {
			evil_sudoku = solveSudoku(evil_sudoku)
		}
		printSudoku(evil_sudoku)
		print("\n")
	}

}



