import scala.annotation.tailrec

val fishes = List(2,1,1,4,4,1,3,4,2,4,2,1,1,4,3,5,1,1,5,1,1,5,4,5,4,1,5,1,3,1,4,2,3,2,1,2,5,5,2,3,1,2,3,3,1,4,3,1,1,1,1,5,2,1,1,1,5,3,3,2,1,4,1,1,1,3,1,1,5,5,1,4,4,4,4,5,1,5,1,1,5,5,2,2,5,4,1,5,4,1,4,1,1,1,1,5,3,2,4,1,1,1,4,4,1,2,1,1,5,2,1,1,1,4,4,4,4,3,3,1,1,5,1,5,2,1,4,1,2,4,4,4,4,2,2,2,4,4,4,2,1,5,5,2,1,1,1,4,4,1,4,2,3,3,3,3,3,5,4,1,5,1,4,5,5,1,1,1,4,1,2,4,4,1,2,3,3,3,3,5,1,4,2,5,5,2,1,1,1,1,3,3,1,1,2,3,2,5,4,2,1,1,2,2,2,1,3,1,5,4,1,1,5,3,3,2,2,3,1,1,1,1,2,4,2,2,5,1,2,4,2,1,1,3,2,5,5,3,1,3,3,1,4,1,1,5,5,1,5,4,1,1,1,1,2,3,3,1,2,3,1,5,1,3,1,1,3,1,1,1,1,1,1,5,1,1,5,5,2,1,1,5,2,4,5,5,1,1,5,1,5,5,1,1,3,3,1,1,3,1)


@tailrec
def populateBrute(fishesList: List[Int], n: Int): List[Int] = {
  if (n==0) fishesList
  else{
    val newFishesNumber = fishesList.count(_==0)
    val newFishesList = fishesList.map(x => if (x==0) 6 else x-1) ++ List.fill(newFishesNumber)(8)
    populateBrute(newFishesList, n-1)
  }
}

def countPopulationForFish(fishDays:Int, days:Int): Long = {
  @tailrec
  def populationForFishWith6Days(cMap: Map[Int,Long], cycles: Int): Map[Int,Long] = {
    if (cycles < 0) cMap
    else {
      val newCMap = Map(
        0 -> (cMap(0) + cMap(7)),
        1 -> (cMap(1) + cMap(8)),
        2 -> (cMap(0) + cMap(2)),
        3 -> (cMap(1) + cMap(3)),
        4 -> (cMap(2) + cMap(4)),
        5 -> (cMap(3) + cMap(5)),
        6 -> (cMap(4) + cMap(6)),
        7 -> cMap(5),
        8 -> cMap(6)
      )
      populationForFishWith6Days(newCMap, cycles-1)
    }
  }

  if (days<=7) {
    populateBrute(List(fishDays), days).length
  }else{
    val reducedDays = days-fishDays-1
    val cycles = reducedDays/7
    val remainedDays = reducedDays%7
    val countMap = Map(0->0L,
      1->0L,
      2->0L,
      3->0L,
      4->0L,
      5->0L,
      6->1L,
      7->0L,
      8->0L)

    val finalMap = populationForFishWith6Days(countMap, cycles)
    finalMap.map{case (k,v) =>
      if(k<7) v*populateBrute(List(k), remainedDays).length else v
    }.sum
  }
}



def countPopulation(fishesList: List[Int], days: Int): Long = {
  if (fishesList.length<=1) {
    if (fishesList.length == 1) countPopulationForFish(fishesList.head, days)
    else 0L
  }
  else{
    val (fishes1, fishes2) = fishesList.splitAt(fishesList.length/2)
    countPopulation(fishes1, days) + countPopulation(fishes2, days)
  }
}


countPopulation(fishes, 256)
