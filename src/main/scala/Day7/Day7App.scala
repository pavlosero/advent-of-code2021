package Day7

import scala.io.Source

object Day7App extends App{

  val filePath = s"${System.getProperty("user.dir")}/src/main/scala/Day7/day7Input.txt"
  val input = Source.fromFile(filePath).getLines.toList.flatMap(_.split(",").map(_.toInt))
  val input2 = List(16,1,2,0,4,2,7,1,2,14)

  def bruteSearch(crabs: List[Int]) = {
    def iter(currMin:Int, currMinArg: Int,  posMinArg: Int): Int = {
      if (posMinArg == -1) currMin
      else {
        val d = crabs.map(x=>math.abs(x-posMinArg)).sum
        val nextMin = Math.min(currMin, d)
        val nextMinArg = if (nextMin < currMin) posMinArg else  currMinArg
        iter(nextMin,nextMinArg, posMinArg-1)
      }
    }

    iter(Int.MaxValue,0, crabs.max)
  }

  def bruteSearch2(crabs: List[Int]) = {
    def iter(currMin:Int, currMinArg: Int,  posMinArg: Int): Int = {
      if (posMinArg == -1) currMin
      else {
        val d = crabs.map(x=>(1 to math.abs(x-posMinArg)).sum).sum
        val nextMin = Math.min(currMin, d)
        val nextMinArg = if (nextMin < currMin) posMinArg else currMinArg
        iter(nextMin,nextMinArg, posMinArg-1)
      }
    }

    iter(Int.MaxValue,0, crabs.max)
  }

  println(bruteSearch2(input))


}