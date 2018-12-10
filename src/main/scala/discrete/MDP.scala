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

    def getDirections(x: Int, y: Int, H: Int): Unit = {
        
        if(valueGrid.length < 1) return;

        val colX = valueGrid(0).length
        val rowY = valueGrid.length

        val tup = (x, y)

        // Corner point condition
        // Use conention left right up down
        tup match {
            case (0, 0)  => "north-west"
            case (0, colX)  => ""
            case
        }

    // Grid world initialization
    def valueOutput(x: Int, y: Int, H: Int): Unit = {
        
    }

    def writeToCSV(df: DataFrame, fileName: DataFrame): Unit = {
        val spark = SparkSession.builder().getOrCreate()
        println("writing dataframe to csv!")
        df.write.format("com.databricks.spark.csv").option("header", "true").save("mydata.csv")
    }
}