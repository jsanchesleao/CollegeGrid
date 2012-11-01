class Materia(val nome: String, val turma: Option[String], val horarios: List[String]){
  override def toString = turma match{
    case None => nome
    case Some(turma) => nome + " turma " + turma
  }
}

class Grid(val materias: List[Materia]) {
  val peso = materias.map( _.horarios.size ).reduce(_+_)
  val hasConflict = materias.combinations(2).toList.exists( x => x(0).horarios.intersect( x(1).horarios ) != Nil )

  val isValid = ! materias.combinations(2).toList.exists(x => x(0).nome == x(1).nome) 

  override def toString = materias.map(_.toString).reduce( _ + "\n" + _)
}

object Reader{

  import scala.io.Source
  def load(file: String): List[Materia] = load(Source.fromFile(file).getLines(), Nil)

  private def load(lines: Iterator[String], materias: List[Materia]): List[Materia] = {
    if(!lines.hasNext){
      materias
    } else {
      load(lines, extract(lines) :: materias)
    }
  }

  private def extract(lines: Iterator[String]): Materia = {
      val desc = lines.next.split(" ").toList
      val horarios = lines.next.split(" ").toList
      if(lines.hasNext) lines.next

      val nome = desc(0)
      val turma = if( desc.size == 1 ) None else Some(desc(1))
      new Materia(nome, turma, horarios)
  }
}

object Calculator{

  def doit: List[Grid] = doit( Reader.load("materias") )
  
  def doit(materias: List[Materia]): List[Grid] = calculate ( createInitialGridList(materias) )

  private def calculate( possibleGrids: List[Grid] ) = findBest( possibleGrids.filter( grid => !grid.hasConflict && grid.isValid ), Nil )

  private def findBest( grids: List[Grid], best: List[Grid] ) = grids.filter( _.peso == bestGridScore(grids) )

  private def bestGridScore( grids: List[Grid] ) = grids.map( _.peso ).sortWith(_ > _).head

  private def createInitialGridList (materias: List[Materia]) = 
    (1 to materias.size).map({ materias.combinations(_).map( x => new Grid(x) ).toList }).flatten.toList

}

object Consulta{
  def apply(arquivo: String){
    println("Melhores Grids Possiveis:\n")
    for(grid <- Calculator.doit( Reader.load(arquivo) ) ){
      println( grid )
      println("----------------")
    }
  }

  def main(args: Array[String]) = apply( args(0) )
}
