import java.io.File
import scala.annotation.tailrec
import scala.io.Source

val inputInstructions = List(
  ("x",655),
  ("y",447),
  ("x",327),
  ("y",223),
  ("x",163),
  ("y",111),
  ("x",81),
  ("y",55),
  ("x",40),
  ("y",27),
  ("y",13),
  ("y",6)
)

val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day13/day13Input.txt"
val input = Source.fromFile(new File(filePath)).getLines().toList
  .map(s=> s.split(",").map(_.toInt))
  .map(s=> (s.head, s(1)))
  .toSet

type Points = Set[(Int,Int)]
type Instructions = List[(String, Int)]

def foldY(points: Points, yFold: Int): Points = {
  val maxY = points.map(_._2).max
  points.filter(_._2 != yFold).map{case (x,y) =>
    val newY = if (yFold < maxY/2) {
      if (y>yFold) y - yFold -1
      else maxY - 2*yFold - 2 - y
    } else {
      if (y>yFold) maxY - y
      else y
    }
    (x,newY)
  }
}

def foldX(points: Points, xFold: Int): Points = {
  val maxX = points.map(_._1).max
  points.filter(_._1 != xFold).map{case (x,y) =>
    val newX = if (xFold < maxX/2) {
      if (x>xFold) x - xFold - 1
      else maxX - 2*xFold - 2 - x
    } else {
      if (x>xFold) maxX - x
      else x
    }
    (newX,y)
  }
}


def printPoints(points: Points): Unit = {
  val maxY = points.map(_._2).max
  val maxX = points.map(_._1).max
  val mat = (for {
    i <- 0 to maxX
    j <- 0 to maxY
    s = if (points.contains((i,j))) "#" else "."
  } yield s).grouped(maxY+1)

  mat.map(_.toList).toList.transpose.foreach(println)
}

@tailrec
def executeFolding(points: Points, instructions: Instructions): Points = {
  if (instructions.isEmpty) points
  else {
    val (orient, pos) = instructions.head
    val newPoints = if (orient == "x") foldX(points, pos) else foldY(points, pos)
    executeFolding(newPoints, instructions.tail)
  }
}

// part 1
executeFolding(input, inputInstructions.take(1)).size

// part 2

val folded = executeFolding(input, inputInstructions)

printPoints(folded)
