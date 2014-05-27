package week3

object dataAbstraction {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(88); 
  val t1 = new NonEmpty(2, new Empty, new Empty);System.out.println("""t1  : week3.NonEmpty = """ + $show(t1 ));$skip(21); 
  val t2 = t1 incl 4;System.out.println("""t2  : week3.IntSet = """ + $show(t2 ));$skip(28); 
  val t1ANDt2 = t1 union t2;System.out.println("""t1ANDt2  : week3.IntSet = """ + $show(t1ANDt2 ));$skip(51); 
  
  def error(msg: String) = throw new Error(msg);System.out.println("""error: (msg: String)Nothing""");$skip(38); val res$0 = 
  
  
  error("TURN DOWN FO WHAAAT?");System.out.println("""res0: Nothing = """ + $show(res$0))}
  
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
