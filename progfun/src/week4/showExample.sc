package week4

object showExample {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + "+" + show(r)
  }                                               //> show: (e: week4.showExample.Expr)String

  show(Sum(Number(1), Number(44)))                //> res0: String = 1+44
}