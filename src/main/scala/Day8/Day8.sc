import scala.io.Source


val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day8/day8Input.txt"
val input = Source.fromFile(filePath).getLines().toList // asdas dsada | dsds
  .map(_.split(" \\| ").map(_.trim)
    .map(_.split(" ")))  // [input , output]
    .map(arr => (arr(0).toList, arr(1).toList) // (input, output)
    )

def simpleRecognizer1478(s: String) = s.length match {
  case 2 => 1
  case 4 => 4
  case 3 => 7
  case 7 => 8
  case _ => -1
}

def coun1478(patterns: List[List[String]]): Int = {
  patterns.map(_.map(simpleRecognizer1478)
    .groupBy(x=>x).map{case (k,v) => (k, v.length)}
  ).foldLeft(0){case (acc, b) =>
    acc + b.filter(_._1 != -1).values.sum
  }
}

//part 1
//coun1478(input.map(_._2))
val dgs = input.head._1.map(_.toSet)
dgs.filter(_.size == 2).head

Set("a", "b", "c").subsetOf(Set("a", "b", "d"))


def processInput(in: List[String], out: List[String]): Int = {
  val digits = in.map(_.toSet)
  val code1 = digits.filter(_.size==2).head
  val code4 = digits.filter(_.size==4).head
  val code7 = digits.filter(_.size==3).head
  val code8 = digits.filter(_.size==7).head

  val len5 = digits.filter(_.size==5)
  val code3 = len5.filter(code1.subsetOf(_)).head
  val code25 = len5.filter(_!=code3)
  val code2 = code25.filter(_.intersect(code4).size == 2).head
  val code5 = code25.filter(_!=code2).head

  val len6 = digits.filter(_.size==6)

  val code9 = len6.filter(code4.subsetOf(_)).head
  val code0 = len6.filter(x=> code1.subsetOf(x) && x!=code9).head
  val code6 = len6.filter(x=> x!=code0 && x!=code9).head

  val digitMap = Map(
    code0 -> 0,
    code1 -> 1,
    code2 -> 2,
    code3 -> 3,
    code4 -> 4,
    code5 -> 5,
    code6 -> 6,
    code7 -> 7,
    code8 -> 8,
    code9 -> 9,
  )

  val outDigits = out.map(_.toSet)

  1000*digitMap(outDigits.head) +
    100*digitMap(outDigits(1)) +
    10*digitMap(outDigits(2)) +
    digitMap(outDigits(3))
}


input.map{case (i,o) => processInput(i,o)}.sum