import java.io.File
import scala.annotation.tailrec
import scala.io.Source

val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day10/day10Input.txt"
val input = Source.fromFile(new File(filePath)).getLines.toList

def chunkCorruptedScore(s: String): (Int, List[Char]) = {
  @tailrec
  def iter(ss: String, queue: List[Char]): (Int, List[Char]) = {
    if (ss.isEmpty) (0, queue)
    else ss.head match {
      case '(' | '[' | '{' | '<' => iter(ss.tail, ss.head +: queue)
      case ')' => if (queue.head!='(') (3, queue) else iter(ss.tail, queue.tail)
      case ']' => if (queue.head!='[') (57, queue) else iter(ss.tail, queue.tail)
      case '}' => if (queue.head!='{') (1197, queue) else iter(ss.tail, queue.tail)
      case '>' => if (queue.head!='<') (25137, queue) else iter(ss.tail, queue.tail)
      case _ => iter(ss.tail, queue)
    }
  }
  iter(s, List())
}

input.map(chunkCorruptedScore(_)._1).sum


//part 2

def remainingChunksScore(list: List[Char]): Long = {
  @tailrec
  def iter(l: List[Char], acc: Long): Long = {
    if (l.isEmpty) acc
    else {
      val nextSum = l.head match {
        case '(' => 1
        case '[' => 2
        case '{' => 3
        case '<' => 4
        case _ => throw new Exception()
      }
      iter(l.tail, 5*acc + nextSum)
    }
  }
  iter(list, 0L)
}


val res = input.map(chunkCorruptedScore)
  .filter(_._1 == 0)
  .map{ case (_,q) => remainingChunksScore(q)}

res.length
res.sorted.reverse((res.length+1)/2 -1)