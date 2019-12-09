// This program solves sudoku puzzles structured as arrays of arrays, 
// with 0 for unknown elements.  The program first tries to solve the
// puzzle with sudoku logic.  If that fails, it employs a backtracking
// algorithm to find a solution (if one exists)

object Sudoku {

	// Function to subset a sudoku (sdf) by a dimension (1=row, 2=col, 3=box) and value
	def filterDim (sdf:Array[Array[Int]], dim:Int, value:Int) : Array[Array[Int]] = {
		var out = Array.ofDim[Int](9, 4)
		var counter = 0;
		for(i <- 0.to(80)) {
			if(sdf(i)(dim) == value) {
				out(counter) = sdf(i)
		        counter += 1
			} 
		}
		return out
	}

	// Function to return list of numbers already in a given dimension (1=row, 2=col, 3=box)
	import scala.collection.mutable.ListBuffer
	def inDim (sdf:Array[Array[Int]], dim:Int, value:Int) : ListBuffer[Int] = {
		var nums = ListBuffer[Int]()
		for(i <- 0.to(80)) {
			if(sdf(i)(dim) == value & sdf(i)(0) != 0) {
				nums += sdf(i)(0)
			} 
		}
		return nums;
	}

	// Function to return a List of what an element can't be
	def cantBesGetter (sdf:Array[Array[Int]], row:Int, col:Int, box:Int) : ListBuffer[Int] = {
		var inrow = inDim(sdf, 1, row)
		var incol = inDim(sdf, 2, col)
		var inbox = inDim(sdf, 3, box)
	    
	    return (inrow ++ incol ++ inbox).distinct
	}

	// Function to get number of unsolved elements in a sudoku
	def numEmpties (sdf:Array[Array[Int]]) : Int = {
		var counter = 0;
		for(i <- 0.to(80)) {
			if(sdf(i)(0) == 0) {
				counter += 1
			}
		}
		return counter
	}

	// Function to populate an element if there is only one number it can't be
	def populateCantBe (sdf:Array[Array[Int]]) : Array[Array[Int]] = {
		for(i <- 0.to(80)) {
			if(sdf(i)(0) == 0) {
				var cantbes = cantBesGetter(sdf, sdf(i)(1), sdf(i)(2), sdf(i)(3))
				if(cantbes.size == 8) {
					sdf(i)(0) = (1.to(9)).diff(cantbes)(0)
				}
			}
		}
		return sdf
	}

	// Function to populate an element if everything else in its dimension cant be a given number
	def populateExclusive (sdf:Array[Array[Int]], element:Int, dim:Int) : Array[Array[Int]] = {

		// Get all numbers that can't be elsewhere in the dimension
		if(sdf(element)(0) == 0) {
			var value = sdf(element)(dim)
		    var inAlready = inDim(sdf, dim, value)
			var out = ListBuffer[Int]()
			for(i <- 0.to(80)) {
				if(sdf(i)(dim) == value & sdf(i)(0) == 0 & i != element) {
					var cantBes = cantBesGetter(sdf, sdf(i)(1), sdf(i)(2), sdf(i)(3))
					cantBes = cantBes.diff(inAlready)
					for(j <- 0.to(cantBes.size - 1)) {
						out += cantBes(j)
					}
				}
			}

			// If there's one number that can't be in any of the other open slots,
			// populate this element with it.
			if(out.size > 0) {
				var mapped = out.groupBy(identity).mapValues(_.size)
				for(j <- out.distinct) {
					if(mapped(j) == (8 - inAlready.size)) {
						sdf(element)(0) = j
					}
				}
			}
		}
		return sdf
	}

	// Function to solve with pure sudoku logic
	def solveLogic (sdf:Array[Array[Int]]) : Array[Array[Int]] = {
		var blanksStart  = numEmpties(sdf)
		if(blanksStart == 0) {
			return sdf
		}
		var blanksFinish = 1
		while(blanksFinish > 0 & blanksStart != blanksFinish) {
			blanksStart = numEmpties(sdf)
			populateCantBe(sdf)
			for(i <- 0.to(80)) {
				if(sdf(i)(0) == 0) {
					for(j <- 1.to(3)) {
				    	if(sdf(i)(0) == 0) {
			    	    	populateExclusive(sdf, i, j)
				        }
				    }
				    blanksFinish = numEmpties(sdf)
				}
			}
		}
		return sdf
	}

	// Function to solve with backtracking
	def solveBacktracking (sdf:Array[Array[Int]]) : Boolean = {
		
		var emptyElements = ListBuffer[Int]()
		for(i <- 0.to(80)) {
			if(sdf(i)(0) == 0) {
				emptyElements += i
			}
		}

		if(emptyElements.size == 0) {
			return true 
		}

	    var index = emptyElements(0)
	    var cantBes = cantBesGetter(sdf, sdf(index)(1), sdf(index)(2), sdf(index)(3))
	    var options = (1.to(9)).toList.diff(cantBes)
	    for(i <- options) {
	    	sdf(index)(0) = i
	    	if(solveBacktracking(sdf)) {
	    		return true
	    	} else {
	    		sdf(index)(0) = 0
	    	} 
	    }
	    return false
	}

	// Full solver.  First try logic, then backtracking
	def solveSudoku (sdf:Array[Array[Int]]) : Array[Array[Int]] = {
		solveLogic(sdf:Array[Array[Int]])
		if(numEmpties(sdf) != 0) {
			solveBacktracking(sdf)
		}
		printSudoku(sdf)
		return sdf
	}

	// Function to print sudoku array in somewhat readable fashion
	def printSudoku (sdf:Array[Array[Int]]) = {

		for(i <- 1.to(9)) {
			var subSdf = filterDim(sdf, 1, i)
			for(j <- 0.to(8)) {
				print(subSdf(j)(0) + " ")
			}
			print("\n")
		}
	}

	// Function to time a function (for benchmarking)
	def time[R](block: => R): R = {
	    val t0 = System.nanoTime()
	    val result = block    // call-by-name
	    val t1 = System.nanoTime()
	    println("Elapsed time: " + (t1 - t0) + "ns")
	    result
	}




	// EXAMPLE

	// Board metadata
	var rows = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 
					1, 2, 3, 4, 5, 6, 7, 8, 9, 
					1, 2, 3, 4, 5, 6, 7, 8, 9,
					1, 2, 3, 4, 5, 6, 7, 8, 9,
					1, 2, 3, 4, 5, 6, 7, 8, 9,
					1, 2, 3, 4, 5, 6, 7, 8, 9,
					1, 2, 3, 4, 5, 6, 7, 8, 9, 
					1, 2, 3, 4, 5, 6, 7, 8, 9, 
					1, 2, 3, 4, 5, 6, 7, 8, 9)
	var cols = rows.sorted
	var boxs = List(1, 1, 1, 4, 4, 4, 7, 7, 7, 
					1, 1, 1, 4, 4, 4, 7, 7, 7, 
					1, 1, 1, 4, 4, 4, 7, 7, 7, 
					2, 2, 2, 5, 5, 5, 8, 8, 8, 
					2, 2, 2, 5, 5, 5, 8, 8, 8, 
					2, 2, 2, 5, 5, 5, 8, 8, 8, 
					3, 3, 3, 6, 6, 6, 9, 9, 9, 
					3, 3, 3, 6, 6, 6, 9, 9, 9, 
					3, 3, 3, 6, 6, 6, 9, 9, 9)

	// Hard Sudoku values
	var vals = List(8, 0, 0, 0, 0, 0, 2, 0, 0, 
					7, 0, 0, 0, 8, 0, 0, 5, 9, 
					0, 0, 0, 9, 0, 2, 0, 0, 6, 
					5, 0, 0, 0, 7, 0, 0, 1, 0, 
					0, 0, 4, 0, 0, 8, 0, 0, 0, 
					0, 8, 0, 0, 0, 6, 0, 0, 0, 
					0, 0, 0, 0, 0, 0, 0, 2, 0, 
					0, 0, 0, 0, 1, 0, 0, 3, 0, 
					3, 7, 0, 0, 6, 0, 0, 0, 0)

	// Create Board
	var hardSudoku = Array.ofDim[Int](81, 4)
	for(i <- 0.to(80)) {
	  hardSudoku(i)(0) = vals(i)
	  hardSudoku(i)(1) = rows(i)
	  hardSudoku(i)(2) = cols(i)
	  hardSudoku(i)(3) = boxs(i)
	}

	// Time a Function
	def time[R](block: => R): R = {
	    val t0 = System.nanoTime()
	    val result = block    // call-by-name
	    val t1 = System.nanoTime()
	    println("Elapsed time: " + (t1 - t0)/1e+9 + " seconds")
	    result
	}

	// Solves in about half a second on macbook air (10X the time of cpp version)
	def main(args:Array[String]) : Unit = {
		time {
			solveSudoku(hardSudoku)
		}
	}

}