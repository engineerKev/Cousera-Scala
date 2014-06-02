package week4

object showExample {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(79); 
  println("Welcome to the Scala worksheet")

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr;$skip(228); 
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + "+" + show(r)
  };System.out.println("""show: (e: week4.showExample.Expr)String""");$skip(36); val res$0 = 

  show(Sum(Number(1), Number(44)));System.out.println("""res0: String = """ + $show(res$0));$skip(216); 
  
  def minimum(leList: List[Int], weight: Int): Int = leList match{
  	case current :: others => if(leList.head < weight) {
  		minimum(leList.tail, leList.head)
  	}else{
  		minimum(leList.tail, weight)
  	}
  };System.out.println("""minimum: (leList: List[Int], weight: Int)Int""");$skip(225); 
  def selSort(unsorted:List[Int], newList:List[Int]): List[Int] = unsorted match {
  	case List() => newList
  	case current :: others => val max = unsorted.max
  		selSort(unsorted.filter(i => i != max), max :: newList)
  };System.out.println("""selSort: (unsorted: List[Int], newList: List[Int])List[Int]""");$skip(46); 
  
  val theList: List[Int] = List(3,4,2,1,6);System.out.println("""theList  : List[Int] = """ + $show(theList ));$skip(32); val res$1 = 
  
  theList.filter(i => i < 6);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(30); val res$2 = 
  
  selSort(theList, List());System.out.println("""res2: List[Int] = """ + $show(res$2))}
}
