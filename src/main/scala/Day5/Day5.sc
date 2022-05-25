import scala.annotation.tailrec
import scala.io.Source

val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day5/day5Input.txt"



case class Point(x: Int, y:Int)

case class Line(p1: Point, p2: Point){
  val x1 = math.min(p1.x, p2.x)
  val x2 = math.max(p1.x, p2.x)
  val y1 = math.min(p1.y, p2.y)
  val y2 = math.max(p1.y, p2.y)

  def isPerpendicular = x1 == x2 || y1 == y2

  def isOnPerpendicularLine(p:Point): Boolean =
    (x1 == x2 && p.x == x1 && p.y >= y1 && p.y <=y2 ) ||
      (y1 == y2 && p.y == y1 && p.x >= x1 && p.x <=x2 )

  def isOnLine(p:Point): Boolean = {
    if (isPerpendicular) isOnPerpendicularLine(p)
    else {
      val xs = if(p1.x < p2.x) p1.x to p2.x else p1.x to p2.x by -1
      val ys = if(p1.y < p2.y) p1.y to p2.y else p1.y to p2.y by -1
      xs.zip(ys).toList.map{case (px,py) => Point(px, py)}.contains(p)
    }
  }
}

type Row2D = List[(Point,Int)]
type Space2D = List[Row2D]
case class Space(space2d: Space2D=List(List())){

  private def expandRow(row:Row2D, maxX: Int, y: Int): Row2D = {
    row++(row.length to maxX).toList.map(x=>(Point(x,y), 0))
  }

  private def expandColumns(space: Space2D, maxX:Int, maxY: Int): Space2D = {
    space++(space.length to maxY).toList.map(
      y=>(0 to maxX).toList.map(
        x=> (Point(x,y), 0)
      ))
  }

  def expandSpace(maxX: Int, maxY: Int): Space = {
    val newX = math.max(maxX, space2d.head.length-1)
    val newY = math.max(maxY, space2d.length-1)
    val expandedOnX = space2d.map(row=>expandRow(row, newX, if(row.isEmpty) 0 else row.head._1.y))
    val expandedOnY = expandColumns(expandedOnX, newX, newY)
    Space(expandedOnY)
  }

  def placeLine(line: Line): Space = {
    val maxX = math.max(line.x1, line.x2)
    val maxY = math.max(line.y1, line.y2)
    val expandedSpace = expandSpace(maxX, maxY)
    Space(
      expandedSpace.space2d.map(row =>
        row.map{case (p, c) => if(line.isOnLine(p)) (p, c+1) else (p,c)}
      )
    )
  }

  override def toString: String = {
    space2d.map(_.foldLeft(""){case (a,b) => s"$a $b"}).foldLeft(""){case (acc, s)=>s"$acc\n$s"}
  }
}



val input = Source.fromFile(filePath).getLines.toList
  .map(_.split("->").map(_.trim))
  .map(_.map(_.split(",").map(_.toInt)))
  .map(coord => Line(Point(coord.head.head, coord.head(1)), Point(coord(1).head, coord(1)(1))))

val perpendiculars = input.filter(_.isPerpendicular)

@tailrec
def placeLines(lines: List[Line], space: Space, dbg: Int): Space = if (lines.isEmpty) space
  else {
    println(dbg)
    placeLines(lines.tail, space.placeLine(lines.head), dbg+1)
  }

//solution 1
//val finalSpace = placeLines(perpendiculars, Space())
//finalSpace.space2d.map(_.count(_._2>1)).sum

// part2
val finalSpace = placeLines(input, Space(), 0)
finalSpace.space2d.map(_.count(_._2>1)).sum

