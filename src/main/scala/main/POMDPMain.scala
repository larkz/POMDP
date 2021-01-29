package main

import discrete.MDP


object POMDPMain extends App {

  override def main(args: Array[String]): Unit = {

    println("Entering main object 2")

    val dummy = args(0)

    // Execute POMDP VI solve

    val mdp = MDP

    val rewardGridInput = Array(
      Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      Array(0.0, 999.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 0.0, -999.0, 0.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    )

    // 5, 4, -0.3
    // 1, 1, 1.0

    val terminalStates = Array((1,1), (3,5) )

    mdp.setRewardGrid(rewardGridInput)
    mdp.setTerminalStatesIndex(terminalStates)
    mdp.visualizeValueGrid()
    mdp.valueIterateFullGrid(150)

    println("reward grid")
    mdp.visualizeRewardGrid()
    println("value grid")
    mdp.visualizeValueGrid()

    val policyGrid = mdp.getVIPolicy(terminalStates)

    for (r <- policyGrid){
      println("|" + r.deep.mkString(", ") + "|" )
    }



  }
}


