class Subject(val name: String, val classroom: Option[String], val time: List[String]){
  override def toString = classroom match{
    case None => name
    case Some(classroom) => name + " class " + classroom
  }
}

class Grid(val subjects: List[Subject]) {
  val timeCharge = subjects.map( _.time.size ).reduce(_+_)
  val hasConflict = subjects.combinations(2).toList.exists( x => x(0).time.intersect( x(1).time ) != Nil )

  val isValid = ! subjects.combinations(2).toList.exists(x => x(0).name == x(1).name) 
  
  def optimize: List[Grid] = findBest ( removeConflictingGrids( subGridList ) )

  private def removeConflictingGrids( possibleGrids: List[Grid] ) = possibleGrids.filter( grid => !grid.hasConflict && grid.isValid )

  private def findBest( grids: List[Grid], best: List[Grid] = Nil ): List[Grid] = grids.filter( _.timeCharge == bestGridScore(grids) )

  private def bestGridScore( grids: List[Grid] ) = grids.map( _.timeCharge ).sortWith(_ > _).head

  private def subGridList = allPossibleCombinations.map( combination => new Grid(combination) )

  private def allPossibleCombinations: List[List[Subject]] = 
    (1 to subjects.size).map( subjects.combinations(_) ).flatten.toList


  override def toString = subjects.map(_.toString).reduce( _ + "\n" + _)
}

object Reader{

  import scala.io.Source
  def load(file: String): List[Subject] = load(Source.fromFile(file).getLines(), Nil)

  private def load(lines: Iterator[String], subjects: List[Subject]): List[Subject] = {
    if(!lines.hasNext){
      subjects
    } else {
      load(lines, extract(lines) :: subjects)
    }
  }

  private def extract(lines: Iterator[String]): Subject = {
      val desc = lines.next.split(" ").toList
      val time = lines.next.split(" ").toList
      if(lines.hasNext) lines.next

      val name = desc(0)
      val classroom = if( desc.size == 1 ) None else Some(desc(1))
      new Subject(name, classroom, time)
  }
}

object Consult{
  def apply(arquivo: String){
    println("Best Possible Grids:\n")
    for(grid <- new Grid(Reader.load(arquivo)).optimize ){
      println( grid )
      println("----------------")
    }
  }

  def main(args: Array[String]) = apply( args(0) )
}
