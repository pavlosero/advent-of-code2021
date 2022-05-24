import scala.annotation.tailrec
import scala.io.Source

val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day3/day3Input.txt"
val input = Source.fromFile(filePath).getLines.toList.map(_.split("").toList)

val columnsList = input.transpose

val mostCommonBits = columnsList.map(column => column.groupBy(x=>x)
  .map{case (key,v) => (key, v.length)}
  .maxBy(_._2)._1)

val lessCommonBits = columnsList.map(column => column.groupBy(x=>x)
  .map{case (key,v) => (key, v.length)}
  .minBy(_._2)._1)

val gamma = Integer.parseInt(mostCommonBits.reduce((a,b)=>s"$a$b"), 2)
val epsilon = Integer.parseInt(lessCommonBits.reduce((a,b)=>s"$a$b"), 2)


// solution 1
gamma*epsilon

// part2
def mostCommonBit(list: List[List[String]], pos: Int): String = {
  val cList = list.transpose
  val occurs = cList(pos).groupBy(x=>x).map{case (k,v)=> (k,v.length)}.toList.sortBy(_._2)
  if (occurs(0)._2 == occurs(1)._2) "1"
  else occurs(1)._1
}

def lessCommonBit(list: List[List[String]], pos: Int): String = {
  val cList = list.transpose
  val occurs = cList(pos).groupBy(x=>x).map{case (k,v)=> (k,v.length)}.toList.sortBy(_._2)
  if (occurs.head._2 == occurs(1)._2) "0"
  else occurs.head._1
}

@tailrec
def selectTheNumber(list: List[List[String]], pos: Int, f: (List[List[String]], Int) => String) : String = {
  if (list.length == 1) list.head.reduce((a,b)=> s"$a$b")
  else {
    val theBit = f(list, pos)
    val filtered = list.filter(s=> s(pos) == theBit)
    selectTheNumber(filtered, pos + 1, f)
  }
}
val org = Integer.parseInt(selectTheNumber(input, 0, mostCommonBit), 2)
val co2 = Integer.parseInt(selectTheNumber(input, 0, lessCommonBit), 2)

org*co2