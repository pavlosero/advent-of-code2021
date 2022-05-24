import scala.io.Source

val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day2/day2Input.txt"
val input = Source.fromFile(filePath).getLines.toList
  .map(x=> x.split(" "))
  .map(arr => (arr(0), arr(1).toInt))

case class SubPosition(x: Int=0, depth: Int=0){
  def subForward(x: Int): SubPosition = SubPosition(this.x+x, this.depth)
  def subDown(depth: Int): SubPosition = SubPosition(this.x, this.depth+depth)
  def subUp(depth: Int): SubPosition = SubPosition(this.x, this.depth-depth)
}



val position = input.foldLeft(SubPosition()){case (oldPosition, (command, value)) =>
  command match {
    case "forward" => oldPosition.subForward(value)
    case "up" => oldPosition.subUp(value)
    case "down" => oldPosition.subDown(value)
  }
}
//answer
position.depth*position.x


//part 2
case class SubPosition2(x: Int=0, depth: Int=0, aim: Int=0){
  def subForward(x: Int): SubPosition2 = SubPosition2(this.x+x, this.depth+this.aim*x, this.aim)
  def subDown(aim: Int): SubPosition2 = SubPosition2(this.x, this.depth, this.aim + aim)
  def subUp(aim: Int): SubPosition2 = SubPosition2(this.x, this.depth, this.aim - aim)
}

val position2 = input.foldLeft(SubPosition2()){case (oldPosition, (command, value)) =>
  command match {
    case "forward" => oldPosition.subForward(value)
    case "up" => oldPosition.subUp(value)
    case "down" => oldPosition.subDown(value)
  }
}
//answer
position2.depth*position2.x