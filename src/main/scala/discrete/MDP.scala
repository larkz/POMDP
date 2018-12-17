package discrete

import scala.util.control.Breaks._

class MDP {

}

object MDP {
    println("Initialize MDP")

    // Bellman Update, Value iteration Algorithm
    // Grid Example
    // Initialize reward matrix, constant action
    // Value iteration

    // Grid World
    // Scala prefers this https://docs.scala-lang.org/style/naming-conventions.html

    var valueGrid = Array[Array[Double]]() // iterative
    var optimalPolicyGrid = Array[Array[Double]]() //iterative
    var rewardGrid = Array[Array[Double]]() //fixed
    var discountFactor = 0.8 //fixed
    var transitionProbabilities = Array[Array[Double]]() //fixed, assume fixed through out

    def getValueGrid(): Array[Array[Double]] = {
        this.valueGrid
    }

    def getOptimalPolicyGrid(): Array[Array[Double]] = {
        this.optimalPolicyGrid
    }

    def setRewardGrid(rGrid: Array[Array[Double]]): Unit = {
        this.rewardGrid = rGrid
        this.valueGrid = Array.ofDim[Double](rGrid(0).length, rGrid.length)
        this.optimalPolicyGrid = Array.ofDim[Double](rGrid(0).length, rGrid.length)
    }

    def setDiscountFactor(dFactor: Double): Unit = {
        this.discountFactor = dFactor
    }

    def getAdjIndices(row: Int, col: Int): Array[Tuple2[Int, Int]] = {
        
        if(rewardGrid.length < 1) return(Array[Tuple2[Int, Int]]())

        val colX = rewardGrid(0).length - 1
        val rowY = rewardGrid.length - 1

        (row, col) match {
            case (0, 0) => Array((0,1), (1,0))
            case (0, `colX`)  => Array((0, colX - 1), (1, colX))
            case (0, _) => Array((0, col + 1), (0, col -1 ), (1, col))
            case (`rowY`, 0)  => Array((rowY - 1, 0), (rowY, 1))
            case (`rowY`, `colX`) => Array((rowY - 1, colX), (rowY, colX - 1))
            case (`rowY`, _) => Array( (row - 1, col), (row, col -1 ), (row, col + 1))
            case (_, `colX`) => Array( (row - 1, col), (row + 1, col ), (row, col - 1))
            case (_, 0) => Array((row, 1), (row - 1, 0), (row + 1, 0) )
            case _ => Array((row -1, col), (row + 1, col), (row, col - 1), (row, col + 1))
        }    
    }

    def getActionIndices(dir: String, row: Int, col: Int): Array[Tuple2[Double, Tuple2[Int, Int]]] = {
        val adjInd = getAdjIndices(row, col)
        var dirInd = Array[Tuple2[Double, Tuple2[Int, Int]]]()

        if (dir == "N"){
            if (adjInd contains (row - 1, col)) dirInd = dirInd ++ Array((0.8, (row - 1, col))) else dirInd = dirInd ++ Array((0.8, (row, col)))
            if (adjInd contains (row, col + 1)) dirInd = dirInd ++ Array((0.2, (row, col + 1))) else dirInd = dirInd ++ Array((0.2, (row, col)))
            if (adjInd contains (row, col - 1)) dirInd = dirInd ++ Array((0.2, (row, col - 1))) else dirInd = dirInd ++ Array((0.2, (row, col)))
        } else if (dir == "S"){
            if (adjInd contains (row + 1, col)) dirInd = dirInd ++ Array((0.8, (row + 1, col))) else dirInd = dirInd ++ Array((0.8, (row, col)))
            if (adjInd contains (row, col + 1)) dirInd = dirInd ++ Array((0.2, (row, col + 1))) else dirInd = dirInd ++ Array((0.2, (row, col)))
            if (adjInd contains (row, col - 1)) dirInd = dirInd ++ Array((0.2, (row, col - 1))) else dirInd = dirInd ++ Array((0.2, (row, col)))
        } else if (dir == "E") {
            if (adjInd contains (row, col + 1)) dirInd = dirInd ++ Array((0.8, (row, col + 1))) else dirInd = dirInd ++ Array((0.8, (row, col)))
            if (adjInd contains (row + 1, col)) dirInd = dirInd ++ Array((0.2, (row + 1, col))) else dirInd = dirInd ++ Array((0.2, (row, col)))
            if (adjInd contains (row - 1, col)) dirInd = dirInd ++ Array((0.2, (row - 1, col))) else dirInd = dirInd ++ Array((0.2, (row, col)))
        } else if (dir == "W") {
            if (adjInd contains (row, col - 1)) dirInd = dirInd ++ Array((0.8, (row, col - 1))) else dirInd = dirInd ++ Array((0.8, (row, col)))
            if (adjInd contains (row + 1, col)) dirInd = dirInd ++ Array((0.2, (row + 1, col))) else dirInd = dirInd ++ Array((0.2, (row, col)))
            if (adjInd contains (row - 1, col)) dirInd = dirInd ++ Array((0.2, (row - 1, col))) else dirInd = dirInd ++ Array((0.2, (row, col)))
        }
        dirInd
    }

    def valueIterateAtIndexWithAction(dir: String, row: Int, col: Int): Unit = {
        val indices = getActionIndices(dir, row, col)
        for (i <- indices){
            val p = i._1
            val t = i._2
            val valueAtIndex = p * (rewardGrid(t._1)(t._2) + discountFactor * valueGrid(t._1)(t._2))
            valueGrid(t._1)(t._2) = valueGrid(row)(col) + valueAtIndex
        }
    }

    def valueIterateAtIndex(row: Int, col: Int): Unit = {
        val actions = Array("N", "S", "E", "W")
        for(a <- actions){
            valueIterateAtIndexWithAction(a, row, col)
        }
    }

    def valueIterateFullGrid(n: Int): Unit = {
        for(i <- 0 to n){
            for(r <- 0 to valueGrid.length - 1 ){
                for(c <- 0 to valueGrid(r).length - 1 ){
                    valueIterateAtIndex(r, c)
                }
            }
        print(valueGrid.deep)
        print("\n")
        }
    }
    
    // Corner point condition
    // Use conention left right up down
    // Grid world initialization
}