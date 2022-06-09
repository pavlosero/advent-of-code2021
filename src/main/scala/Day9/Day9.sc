import scala.io.Source

val filePath = s"${System.getProperty("user.dir")}/Documents/Studying/Advent of Code/AdventOfCode/src/main/scala/Day9/day9Input.txt"
val input = Source.fromFile(filePath).getLines.map(_.split("").map(_.toInt)).toArray

def isLocalMin(arr:Array[Array[Int]], posX: Int, posY: Int): Boolean = {
  val v = arr(posX)(posY)
  (v<arr(posX)(posY-1)) && (v<arr(posX-1)(posY)) && (v<arr(posX)(posY+1)) && (v<arr(posX+1)(posY))
}


def padArray(arr:Array[Array[Int]]): Array[Array[Int]] = {
  val nCols = arr(0).length
  Array(Array.fill(nCols+2)(9)) ++
    arr.map(Array(9) ++ _ ++ Array(9)) ++
    Array(Array.fill(nCols+2)(9))

}

def findBasinSize(arr:Array[Array[Int]], minX: Int, minY: Int): Int = {
  val nRows = arr.length
  val nCols = arr(0).length
  def checkAllDirections(x: Int, y: Int, prev: Int, acc: Set[(Int,Int)]): Set[(Int, Int)] = {
    val v = arr(x)(y)
    if(x==0 || x==nRows-1 || y==0 || y==nCols-1 ||
      arr(x)(y)==9 || v<prev || acc.contains((x,y))) acc
    else {
      val newAcc = acc ++ Set((x,y))
      checkAllDirections(x-1, y, v, newAcc) ++
        checkAllDirections(x, y-1, v, newAcc) ++
        checkAllDirections(x+1, y, v, newAcc) ++
        checkAllDirections(x, y+1, v, newAcc)
    }
  }

  checkAllDirections(minX, minY, arr(minX)(minY), Set()).size
}

def findLocalMins(paddedArr: Array[Array[Int]]) = for {
  i <- 1 until paddedArr.length-1
  j <- 1 until paddedArr(0).length-1
  if isLocalMin(paddedArr, i, j)
}yield (i,j)

def findBasinSizes(arr: Array[Array[Int]], mins: Seq[(Int,Int)]): Seq[((Int, Int), Int)] = mins.map{case (x,y) => ((x,y), findBasinSize(arr, x, y))}


val paddedInput = padArray(input)

val localMins = for {
  i <- 1 until paddedInput.length-1
  j <- 1 until paddedInput(0).length-1
  if isLocalMin(paddedInput, i, j)
}yield (i,j)


val mapBasins = findBasinSizes(paddedInput, localMins)

mapBasins.sortBy(_._2).reverse.take(3).map(_._2).product