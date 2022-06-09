import scala.annotation.tailrec

val input = List(
  ("xx","end"),
  ("EG","xx"),
  ("iy","FP"),
  ("iy","qc"),
  ("AB","end"),
  ("yi","KG"),
  ("KG","xx"),
  ("start","LS"),
  ("qe","FP"),
  ("qc","AB"),
  ("yi","start"),
  ("AB","iy"),
  ("FP","start"),
  ("iy","LS"),
  ("yi","LS"),
  ("xx","AB"),
  ("end","KG"),
  ("iy","KG"),
  ("qc","KG"),
  ("FP","xx"),
  ("LS","qc"),
  ("FP","yi")
)

def explore(nodes: List[(String, String)]): List[List[String]] = {
  def checkForNewCaves(path: List[String]): Option[List[String]] = {
    val lastCave = path.head
    if (lastCave == "end") None
    else{
      val possibleNexts = nodes
        .filter(n=> n._1 == lastCave || n._2 == lastCave)
        .map(n => if (n._1 == lastCave) n._2 else n._1)
        .filter(n => n!="start" && (n.head.isUpper || (n.head.isLower && !path.contains(n))))

      if(possibleNexts.isEmpty) None
      else Some(possibleNexts)
    }
  }

  @tailrec
  def iterate(paths: List[List[String]]): List[List[String]] = {
    val possibleNextPerPath = paths.map(p => checkForNewCaves(p))
    if (possibleNextPerPath.flatten.isEmpty) paths
    else {
      val newPath = paths.zip(possibleNextPerPath)
        .flatMap{case (path, posCave) => posCave match {
          case None => List(path)
          case Some(caves) => caves.map(_+:path)
        }
      }
      iterate(newPath)
    }
  }

  iterate(List(List("start")))

}

explore(input).count(_.head == "end")


// part 2

def explore2(nodes: List[(String, String)]): List[List[String]] = {

  def singleCaveTwice(path: List[String]): Boolean = path.filter(n => n.head.isLower && n!="start" )
    .map((_,1))
    .groupBy(_._1)
    .map{case (node, occ) => (node, occ.map(_._2).sum)}
    .exists(_._2 == 2)


  def checkForNewCaves(path: List[String]): Option[List[String]] = {
    val lastCave = path.head
    if (lastCave == "end") None
    else{
      val possibleNexts = nodes
        .filter(n=> n._1 == lastCave || n._2 == lastCave)
        .map(n => if (n._1 == lastCave) n._2 else n._1)
        .filter(n => n!="start" &&
          (n.head.isUpper ||
            (n.head.isLower && singleCaveTwice(path) && !path.contains(n)) ||
            (n.head.isLower && !singleCaveTwice(path) && path.count(_==n)<2)
          )
      )

      if(possibleNexts.isEmpty) None
      else Some(possibleNexts)
    }
  }

  @tailrec
  def iterate(paths: List[List[String]]): List[List[String]] = {
    val possibleNextPerPath = paths.map(p => checkForNewCaves(p))
    if (possibleNextPerPath.flatten.isEmpty) paths
    else {
      val newPath = paths.zip(possibleNextPerPath)
        .flatMap{case (path, posCave) => posCave match {
          case None => List(path)
          case Some(caves) => caves.map(_+:path)
        }
        }
      iterate(newPath)
    }
  }

  iterate(List(List("start")))

}


explore2(input).count(_.head == "end")