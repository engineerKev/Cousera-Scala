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

  def minimum(leList: List[Int], weight: Int): Int = leList match {
    case current :: others => if (leList.head < weight) {
      minimum(leList.tail, leList.head)
    } else {
      minimum(leList.tail, weight)
    }
  }                                               //> minimum: (leList: List[Int], weight: Int)Int
  def selSort(unsorted: List[Int], newList: List[Int]): List[Int] = unsorted match {
    case List() => newList
    case current :: others =>
      val max = unsorted.max
      selSort(unsorted.filter(i => i != max), max :: newList)
  }                                               //> selSort: (unsorted: List[Int], newList: List[Int])List[Int]

  val theList: List[Int] = List(3, 4, 2, 1, 6)    //> theList  : List[Int] = List(3, 4, 2, 1, 6)

  theList.filter(i => i < 6)                      //> res1: List[Int] = List(3, 4, 2, 1)

  selSort(theList, List())                        //> res2: List[Int] = List(1, 2, 3, 4, 6)

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
}