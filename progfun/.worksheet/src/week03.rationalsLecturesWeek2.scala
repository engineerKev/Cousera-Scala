package week03

object rationalsLecturesWeek2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(75); 
  val x = new Rational(1,3);System.out.println("""x  : week03.Rational = """ + $show(x ));$skip(10); val res$0 = 
  x.numer;System.out.println("""res0: Int = """ + $show(res$0));$skip(10); val res$1 = 
  x.denom;System.out.println("""res1: Int = """ + $show(res$1));$skip(31); 
  
  val y = new Rational(5,7);System.out.println("""y  : week03.Rational = """ + $show(y ));$skip(8); val res$2 = 
  x + y;System.out.println("""res2: week03.Rational = """ + $show(res$2));$skip(31); 
  
  val z = new Rational(3,2);System.out.println("""z  : week03.Rational = """ + $show(z ));$skip(15); val res$3 = 
  
  x - y - z;System.out.println("""res3: week03.Rational = """ + $show(res$3));$skip(7); val res$4 = 
  y +y;System.out.println("""res4: week03.Rational = """ + $show(res$4));$skip(8); val res$5 = 
  x < y;System.out.println("""res5: Boolean = """ + $show(res$5));$skip(11); val res$6 = 
  x.max(y);System.out.println("""res6: week03.Rational = """ + $show(res$6));$skip(18); val res$7 = 
  new Rational(2);System.out.println("""res7: week03.Rational = """ + $show(res$7))}
  
  
}

class Rational(x:Int, y:Int) {
	require(y != 0, "denominator must be nonzero")
	
	def this(x:Int) = this(x,1)
	
	private def gcd(a:Int, b:Int): Int = if(b== 0) a else gcd(b,a%b)
	private val g = gcd(x,y)
	def numer = x
	def denom = y
	
	def < (that:Rational) = this.numer * that.denom < that.numer * this.denom
	
	def max(that:Rational) = if(this < (that)) that else this
	
	def +(that: Rational) =
		new Rational(numer * that.denom + that.numer * denom,
		denom * that.denom)
		
	override def toString = numer/g + "/" + denom/g
	
	def unary_- :Rational = new Rational(-numer, denom)
	
	def -(that:Rational) =
		this + -that
}
