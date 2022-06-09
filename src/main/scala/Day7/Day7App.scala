package Day7

import scala.io.Source

object Day7App extends App{

  val filePath = s"${System.getProperty("user.dir")}/src/main/scala/Day7/day7Input.txt"
  val input = Source.fromFile(filePath).getLines.toList.flatMap(_.split(",").map(_.toInt))
  val input2 = List(16,1,2,0,4,2,7,1,2,14)
  val input3 = List(13,41, 202, 293, 439, 10, 102, 10, 13, 10, 10, 10, 19, 19)


  println(bruteSearch(input))
  println(goodSearch(input))

  def goodSearch(crabs: List[Int]): (Int, Int)  = {
    def fuel(pos: Int) = crabs.map(x=>math.abs(x-pos)).sum
    val sorted = crabs.sorted
    if (sorted.length%2 == 1){
      val med = sorted((sorted.length+1)/2 - 1)
      (med, fuel(med))
    }else{
      val meds = List(sorted(sorted.length/2-1 ), sorted(sorted.length/2))
      val fuels = List(fuel(meds.head), fuel(meds(1)))
      meds.zip(fuels).minBy(_._2)
    }


  }


  def bruteSearch(crabs: List[Int]) = {
    def iter(currMin:Int, currMinArg: Int,  posMinArg: Int): Int = {
      if (posMinArg == -1) currMin
      else {
        val d = crabs.map(x=>math.abs(x-posMinArg)).sum
        val nextMin = Math.min(currMin, d)
        val nextMinArg = if (nextMin <= currMin) posMinArg else  currMinArg
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