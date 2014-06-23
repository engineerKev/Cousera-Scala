package week7

object streamsTests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val testVec = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
                                                  //> testVec  : scala.collection.immutable.Vector[scala.collection.immutable.Vect
                                                  //| or[Char]] = Vector(Vector(S, T), Vector(o, o), Vector(o, o))
  val posVec = testVec.filter(v=>v.contains('o')) //> posVec  : scala.collection.immutable.Vector[scala.collection.immutable.Vecto
                                                  //| r[Char]] = Vector(Vector(o, o), Vector(o, o))
  for{
  	v <- posVec
  	if(v.contains('o'))
  }yield (v.indexOf('o'))                         //> res0: scala.collection.immutable.Vector[Int] = Vector(0, 0)
  testVec.head.size                               //> res1: Int = 2
}