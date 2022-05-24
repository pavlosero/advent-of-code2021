import scala.annotation.tailrec
import scala.io.Source

val filePath1 = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day4/day4Input1.txt"
val filePath2 = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day4/day4Input2.txt"
val bingoNumbers = Source.fromFile(filePath2).getLines.toList.flatMap(_.split(","))

case class BingoTicket(row1:List[(String, Boolean)],
                       row2:List[(String, Boolean)],
                       row3:List[(String, Boolean)],
                       row4:List[(String, Boolean)],
                       row5:List[(String, Boolean)]){

  private def crossRow(row: List[(String, Boolean)], s: String) : List[(String, Boolean)] = {
    row.map{case (num, c) => if (num==s) (num,true) else (num,c)}
  }

  def crossNumber(s: String): BingoTicket = BingoTicket(
    crossRow(this.row1, s),
    crossRow(this.row2, s),
    crossRow(this.row3, s),
    crossRow(this.row4, s),
    crossRow(this.row5, s)
  )

  def checkVictory(): Boolean = {
    val matrix = toMatrix
    val rowCheck = matrix.exists(_.forall(_._2))
    val columnCheck = matrix.transpose.exists(_.forall(_._2))
    rowCheck || columnCheck
  }

  def toMatrix: List[List[(String,Boolean)]] = List(row1, row2, row3, row4, row5)
}

val bingoTickets = Source.fromFile(filePath1).getLines.toList.filter(_!="")
  .sliding(5,5).toList // to chunk of 5
  .map(_.map(_.split(" ").filter(_ != "").toList.map((_,false))))
  .map(mat=>BingoTicket(mat.head, mat(1), mat(2), mat(3), mat(4)))



def score(winner: BingoTicket, lastNumber: Int): Int = {
  val boardScore = winner.toMatrix.flatten.filterNot(_._2).map(_._1.toInt).sum
  boardScore*lastNumber
}

@tailrec
def findWinningTicketScore(tickets: List[BingoTicket], numbers: List[String]): Option[Int] = {
  if (numbers.isEmpty) return None
  val lastNumber = numbers.head
  val crossedTickets = tickets.map(_.crossNumber(lastNumber))
  val winner = crossedTickets.find(_.checkVictory())
  winner match {
    case Some(t) => Some(score(t, lastNumber.toInt))
    case None => findWinningTicketScore(crossedTickets, numbers.tail)
  }

}
findWinningTicketScore(bingoTickets, bingoNumbers)


// part2
def findLastWinningTicketScore(tickets: List[BingoTicket], numbers: List[String]): Option[Int] = {
  if (numbers.isEmpty) return None
  else{
    val lastNumber = numbers.head
    val crossedTickets = tickets.map(_.crossNumber(lastNumber))
    val (winner, losers) = crossedTickets.span(_.checkVictory())
    if (losers.isEmpty) Some(score(winner.head, lastNumber.toInt))
    else findLastWinningTicketScore(losers, numbers.tail)
  }
}

findLastWinningTicketScore(bingoTickets, bingoNumbers)