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

    def getRewardFromGrid(row: Int, col: Int): Array[Tuple2[Double, Double]] = {
        
        if(valueGrid.length < 1) return(Array[Tuple2[Double, Double]]())

        val colX = valueGrid(0).length - 1
        val rowY = valueGrid.length - 1
        val tup = (row, col)
        println(tup)

        tup match {
            case (0, 0) => Array((0,1), (1,0))
            case (0, colX)  => Array((0, colX - 1), (1, colX))
            case (rowY, 0)  => Array((rowY - 1, 0), (rowY, 1))
            case (rowY, colX) => Array((rowY - 1, colX), (rowY, colX - 1))
            case (0, _) => Array((0, col + 1), (0, col -1 ), (1, col))
            case (rowY, _) => Array( (row - 1, col), (row, col -1 ), (row, col + 1))
            case (_, colX) => Array( (row - 1, col), (row + 1, col ), (row, col - 1))
            case (_, 0) => Array((row, 1), (row - 1, 0), (row + 1, 0) )
            case _ => Array((row -1, col), (row + 1, col), (row, col - 1), (row, col - 1))
        }
    }
        
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