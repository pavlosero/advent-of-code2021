import scala.annotation.tailrec

val input = Seq(
  Seq(4,1,3,4,3,8,4,6,2,6),
  Seq(7,1,1,4,5,8,5,2,5,7),
  Seq(1,5,8,2,5,3,6,4,8,8),
  Seq(4,8,6,5,7,1,5,5,3,8),
  Seq(5,7,3,3,4,2,3,5,1,3),
  Seq(8,5,3,2,1,4,4,1,8,1),
  Seq(1,2,8,8,6,1,4,5,8,3),
  Seq(2,2,4,8,7,1,1,1,4,1),
  Seq(6,4,1,5,8,7,1,6,8,1),
  Seq(7,8,8,1,5,3,1,4,3,8)
)



def induceFlashes(seq: Seq[Seq[Int]]): (Seq[Seq[Int]], Int) = {

  def zeroPaddedSeq(ss: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val nCols = ss.head.length+2
    Seq.fill(nCols)(0) +: ss.map(Seq(0) ++ _ ++ Seq(0)) :+ Seq.fill(nCols)(0)
  }

  def count10InNeighbours(seq: Seq[Seq[Int]], x:Int, y:Int): Int = {
    val n = seq.slice(x-1, x+2).map(_.slice(y - 1, y + 2).count(_ == 10)).sum
    if (seq(x)(y) == 10) n-1 else n
  }

  @tailrec
  def induce(ss: Seq[Seq[Int]]): (Seq[Seq[Int]], Int) = {
    if (ss.map(_.count(_==10)).sum == 0) {
      (ss.map(_.map(a => if (a > 9) 0 else a)), ss.map(_.count(_==100)).sum)
    }
    else {
      val paddedSS = zeroPaddedSeq(ss)
      val nCols = paddedSS.head.length
      val nRows = paddedSS.length
      val induced = for{
        i <- 1 until nRows-1
        j <- 1 until nCols-1
        nFlashed = count10InNeighbours(paddedSS, i, j)
        inducedV = if (paddedSS(i)(j) < 10 ) paddedSS(i)(j) + nFlashed else 100
        optV = if (inducedV != 100 && inducedV>10) 10 else inducedV
      } yield optV
      val nFlashed = induced.count(_==100)
      induce(induced.grouped(nCols-2).map(_.toSeq).toSeq)
    }
  }

  induce(seq)
}

@tailrec
def repeatSteps(seq: Seq[Seq[Int]], steps: Int, flashes: Int): (Seq[Seq[Int]], Int) = {
  if(steps == 0) (seq, flashes)
  else{
    val add1 = seq.map(_.map(_+1))
    val (induced, newFlashes) = induceFlashes(add1)
    repeatSteps(induced, steps-1, flashes+newFlashes)
  }
}


val dbg = Seq(
  Seq(5,4,8,3,1,4,3,2,2,3),
  Seq(2,7,4,5,8,5,4,7,1,1),
  Seq(5,2,6,4,5,5,6,1,7,3),
  Seq(6,1,4,1,3,3,6,1,4,6),
  Seq(6,3,5,7,3,8,5,4,7,8),
  Seq(4,1,6,7,5,2,4,6,4,5),
  Seq(2,1,7,6,8,4,1,7,2,1),
  Seq(6,8,8,2,8,8,1,1,3,4),
  Seq(4,8,4,6,8,4,8,5,5,4),
  Seq(5,2,8,3,7,5,1,5,2,6)
)

val (end, c) = repeatSteps(input, 100, 0)
end.foreach(println)
c

// part 2

@tailrec
def firstFullSlash(seq: Seq[Seq[Int]], steps: Int): (Seq[Seq[Int]], Int) = {
  if (seq.map(_.sum).sum == 0) (seq, steps)
  else{
    val add1 = seq.map(_.map(_+1))
    val (induced, newFlashes) = induceFlashes(add1)
    firstFullSlash(induced, steps+1)
  }
}

val (end2, c2) = firstFullSlash(input, 0)
end2.foreach(println)
c2