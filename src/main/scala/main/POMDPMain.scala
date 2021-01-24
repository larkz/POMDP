package main

import discrete.MDP


object POMDPMain extends App {

  override def main(args: Array[String]): Unit = {

    println("Entering main object 2")

    val dummy = args(0)

    // Execute POMDP VI solve

    val mdp = MDP
    val rewardGridInput = Array(Array(1.0, 0.0, 1.0), Array(0.0, 0.0, -1.0))
    mdp.setRewardGrid(rewardGridInput)
    mdp.valueIterateFullGrid(15)

  }
}


