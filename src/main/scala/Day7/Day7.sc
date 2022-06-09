val input2 = List(16,1,2,0,4,2,7,1,2,14)
val (s,n) = input2.map(_+1).groupBy(x=>x)
  .map{case (x, l) => (x,l.length)}.toList
  .reduce{case (a: (Int, Int),b: (Int, Int)) => (a._1*a._2 + b._1*b._2, a._2 + b._2)}