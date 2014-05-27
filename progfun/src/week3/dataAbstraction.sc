package week3

object dataAbstraction {
  val t1 = new NonEmpty(2, new Empty, new Empty)  //> t1  : week3.NonEmpty = {.2.}
  val t2 = t1 incl 4                              //> t2  : week3.IntSet = {.2{.4.}}
  val t1ANDt2 = t1 union t2                       //> t1ANDt2  : week3.IntSet = {.2{.4.}}
  
  def error(msg: String) = throw new Error(msg)   //> error: (msg: String)Nothing
  
  
  error("TURN DOWN FO WHAAAT?")                   //> java.lang.Error: TURN DOWN FO WHAAAT?
                                                  //| 	at week3.dataAbstraction$$anonfun$main$1.error$1(week3.dataAbstraction.s
                                                  //| cala:8)
                                                  //| 	at week3.dataAbstraction$$anonfun$main$1.apply$mcV$sp(week3.dataAbstract
                                                  //| ion.scala:11)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week3.dataAbstraction$.main(week3.dataAbstraction.scala:3)
                                                  //| 	at week3.dataAbstraction.main(week3.dataAbstraction.scala)
  
}

abstract class IntSet {
	def incl(x:Int):IntSet
	def contains(x: Int):Boolean
	def union(other: IntSet):IntSet
}

class Empty extends IntSet {
	def contains(x:Int): Boolean = false
	def incl(x:Int): IntSet = new NonEmpty(x,new Empty, new Empty)
	override def toString = "."
	def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left:IntSet, right:IntSet) extends IntSet {
	def contains(x: Int): Boolean =
		if(x<elem) left contains x
		else if (x > elem) right contains x
		else true
		
	def incl(x:Int): IntSet =
		if(x<elem) new NonEmpty(elem, left incl x, right)
		else if(x>elem) new NonEmpty(elem,left,right incl x)
		else this
		
	def union(other: IntSet): IntSet = ((left union right) union other) incl elem
		
	override def toString = "{" + left + elem + right + "}"
}