package week7

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val problems = new Pouring(Vector(4,9, 19))     //> problems  : week7.Pouring = week7.Pouring@57b5b346
  problems.moves                                  //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with w
                                                  //| eek7.test.problems.Move] = Vector(Empty(0), Empty(1), Empty(2), Fill(0), Fil
                                                  //| l(1), Fill(2), Pour(0,1), Pour(0,2), Pour(1,0), Pour(1,2), Pour(2,0), Pour(2
                                                  //| ,1))
  problems.pathSets.take(3).toList                //> res1: List[Set[week7.test.problems.Path]] = List(Set(-->Vector(0, 0, 0)), Se
                                                  //| t(Fill(0)-->Vector(4, 0, 0), Fill(1)-->Vector(0, 9, 0), Fill(2)-->Vector(0, 
                                                  //| 0, 19)), Set(Fill(1) Fill(0)-->Vector(4, 9, 0), Fill(1) Pour(1,2)-->Vector(0
                                                  //| , 0, 9), Fill(0) Pour(0,1)-->Vector(0, 4, 0), Fill(1) Pour(1,0)-->Vector(4, 
                                                  //| 5, 0), Fill(2) Fill(0)-->Vector(4, 0, 19), Fill(0) Fill(2)-->Vector(4, 0, 19
                                                  //| ), Fill(1) Fill(2)-->Vector(0, 9, 19), Fill(2) Pour(2,0)-->Vector(4, 0, 15),
                                                  //|  Fill(0) Fill(1)-->Vector(4, 9, 0), Fill(0) Pour(0,2)-->Vector(0, 0, 4), Fil
                                                  //| l(2) Fill(1)-->Vector(0, 9, 19), Fill(2) Pour(2,1)-->Vector(0, 9, 10)))
  
  
  problems.solutions(17)                          //> res2: Stream[week7.test.problems.Path] = Stream(Fill(1) Fill(0) Pour(0,2) Fi
                                                  //| ll(0) Pour(0,2) Pour(1,2)-->Vector(0, 0, 17), ?)
}
  