package week03

object rationalsLecturesWeek2 {
  val x = new Rational(1,3)                       //> x  : week03.Rational = 1/3
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  
  val y = new Rational(5,7)                       //> y  : week03.Rational = 5/7
  x + y                                           //> res2: week03.Rational = 22/21
  
  val z = new Rational(3,2)                       //> z  : week03.Rational = 3/2
  
  x - y - z                                       //> res3: week03.Rational = -79/42
  y +y                                            //> res4: week03.Rational = 10/7
  x < y                                           //> res5: Boolean = true
  x.max(y)                                        //> res6: week03.Rational = 5/7
  new Rational(2)                                 //> res7: week03.Rational = 2/1
  
  
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