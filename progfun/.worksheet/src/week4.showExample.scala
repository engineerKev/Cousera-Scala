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

  show(Sum(Number(1), Number(44)));System.out.println("""res0: String = """ + $show(res$0))}
}
