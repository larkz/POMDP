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
    var discountFactor = 1.0 //fixed
    var transitionProbabilities = Array[Array[Double]]() //fixed, assume fixed through out

    def getValueGrid(): Array[Array[Double]] = {
        this.valueGrid
    }

    def getOptimalPolicyGrid(): Array[Array[Double]] = {
        this.optimalPolicyGrid
    }

    def setRewardGrid(rGrid: Array[Array[Double]]): Unit = {
        this.rewardGrid = rGrid
        this.valueGrid = Array.ofDim[Int](rGrid(0).length, rGrid.length)
        this.optimalPolicyGrid = Array.ofDim[Int](rGrid(0).length, rGrid.length)
    }

    def setDiscountFactor(dFactor: Double): Unit = {
        this.discountFactor = dFactor
    }

    def getAdjIndices(row: Int, col: Int): Array[Tuple2[Int, Int]] = {
        
        if(rewardGrid.length < 1) return(Array[Tuple2[Int, Int]]())

        val colX = rewardGrid(0).length - 1
        val rowY = rewardGrid.length - 1

        (row, col) match {
            case (0, 0) => println("case 1"); Array((0,1), (1,0))
            case (0, `colX`)  => println("case 2"); Array((0, colX - 1), (1, colX))
            case (0, _) => println("case 5"); Array((0, col + 1), (0, col -1 ), (1, col))
            case (`rowY`, 0)  => println("case 3"); Array((rowY - 1, 0), (rowY, 1))
            case (`rowY`, `colX`) => println("case 4"); Array((rowY - 1, colX), (rowY, colX - 1))
            case (`rowY`, _) => Array( (row - 1, col), (row, col -1 ), (row, col + 1))
            case (_, `colX`) => Array( (row - 1, col), (row + 1, col ), (row, col - 1))
            case (_, 0) => Array((row, 1), (row - 1, 0), (row + 1, 0) )
            case _ => Array((row -1, col), (row + 1, col), (row, col - 1), (row, col + 1))
        }    
    }

    def getActionIndices(dir: String, row: Int, col: Int): Array[Tuple2[Int, Int]] = {
        val adjInd = getAdjIndices(row, col)
        var dirInd = Array[Tuple2[Int, Int]]()
        if (dir == "N"){
            if (adjInd contains (row - 1, col)) dirInd = dirInd ++ Array((row - 1, col))
            if (adjInd contains (row, col + 1)) dirInd = dirInd ++ Array((row, col + 1))
            if (adjInd contains (row, col - 1)) dirInd = dirInd ++ Array((row, col - 1))
        } else if (dir == "S"){
            if (adjInd contains (row + 1, col)) dirInd = dirInd ++ Array((row + 1, col))
            if (adjInd contains (row, col + 1)) dirInd = dirInd ++ Array((row, col + 1))
            if (adjInd contains (row, col - 1)) dirInd = dirInd ++ Array((row, col - 1))
        } else if (dir == "E") {
            if (adjInd contains (row + 1, col)) dirInd = dirInd ++ Array((row + 1, col))
            if (adjInd contains (row - 1, col)) dirInd = dirInd ++ Array((row - 1, col))
            if (adjInd contains (row, col + 1)) dirInd = dirInd ++ Array((row, col + 1))
        } else if (dir == "W") {
            if (adjInd contains (row + 1, col)) dirInd = dirInd ++ Array((row + 1, col))
            if (adjInd contains (row - 1, col)) dirInd = dirInd ++ Array((row - 1, col))
            if (adjInd contains (row, col - 1)) dirInd = dirInd ++ Array((row, col - 1))
        }
        dirInd
    }



    def valueIterate(row: Int, col: Int): Unit = {
        val indices = getAdjIndices(row, col)
        for (t <- indices){
            valueGrid(t._1)(t._2) = rewardGrid(t._1)(t._2) + discountFactor * valueGrid(t._1)(t._2)
        }
    }

    def getRewardFromGrid(row: Int, col: Int): Array[Double] = {
        val indices = getAdjIndices(row, col)
        indices.map(t => rewardGrid(t._1)(t._2))
    }

    def getValueFromGrid(row: Int, col: Int): Array[Double] = {
        val indices = getAdjIndicies(row, col)
        indices.map(t => valueGrid(t._1)(t._2))
    }

    
    


        // Corner point condition
        // Use conention left right up down
        


    // Grid world initialization
    def valueOutput(x: Int, y: Int, H: Int): Unit = {
        
    }

    def writeToCSV(df: DataFrame, fileName: DataFrame): Unit = {
        val spark = SparkSession.builder().getOrCreate()
        println("writing dataframe to csv!")
        df.write.format("com.databricks.spark.csv").option("header", "true").save("mydata.csv")
    }
}