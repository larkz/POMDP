package main

import discrete.MDP
import org.apache.log4j.Logger

class POMDPMain extends Serializable {
  @transient lazy val logger: Logger = Logger.getLogger(getClass.getName)
}

object POMDPMain extends Serializable{

  def main(args: Array[String]): Unit = {

    println("testing main")
    val dummy = args(0)
    dummy match {
      case _ => throw new ClassNotFoundException(s"$dummy class does not exist !")
    }
  }
}


