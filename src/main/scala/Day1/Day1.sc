import scala.io.Source

val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day1/day1Input.txt"
val input = Source.fromFile(filePath).getLines.toList.map(_.toInt)
val nextItems = input.splitAt(1)._2 :+ 0

// solution1
 input.zip(nextItems).count { case (a, b) => b > a }

val next2Items = nextItems.splitAt(1)._2 :+ 0

// solution2

val sumList = input.zip(nextItems).map{case (a,b) => a+b}
  .zip(next2Items).map{case(a,b)=>a+b}

val nextSumItems = sumList.splitAt(1)._2 :+ 0

sumList.zip(nextSumItems).count { case (a, b) => b > a }
