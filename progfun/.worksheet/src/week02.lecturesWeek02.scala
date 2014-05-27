package week02

object lecturesWeek02 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(205); 
  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  };System.out.println("""sum: (f: Int => Int, a: Int, b: Int)Int""");$skip(24); val res$0 = 
  sum(x => x * x, 3, 5);System.out.println("""res0: Int = """ + $show(res$0));$skip(176); 

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b));System.out.println("""mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int)Int""");$skip(96); 
    
  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b);System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(28); val res$1 = 
  product(x => x * x)(3, 4);System.out.println("""res1: Int = """ + $show(res$1));$skip(49); 

  def factorial(n: Int) = product(x => x)(1, n);System.out.println("""factorial: (n: Int)Int""");$skip(15); val res$2 = 
  factorial(5)
  
  import math.abs;System.out.println("""res2: Int = """ + $show(res$2));$skip(46); 
  val tolerance = 0.0001;System.out.println("""tolerance  : Double = """ + $show(tolerance ));$skip(77); 
  def isCloseEnough(x:Double, y: Double) =
  	abs((x-y) / x) / x < tolerance;System.out.println("""isCloseEnough: (x: Double, y: Double)Boolean""");$skip(264); 
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess:Double): Double = {
  		println( "calling f with " + guess )
  		val next = f(guess)
  		if (isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  };System.out.println("""fixedPoint: (f: Double => Double)(firstGuess: Double)Double""");$skip(28); val res$3 = 
	fixedPoint(x=> 1 + x/2)(1);System.out.println("""res3: Double = """ + $show(res$3));$skip(97); 
	
	def averageDamp(f: Double => Double)(x:Double) = {
	  println( "x = " + x )
	  (x+f(x))/2
  };System.out.println("""averageDamp: (f: Double => Double)(x: Double)Double""");$skip(50); 
      
  def ad( arg: Double ) : Double = { arg };System.out.println("""ad: (arg: Double)Double""");$skip(44); 
  
  println( averageDamp(y => 3/y)( 3.3) );$skip(284); 
                                                             
	def sqrt(x:Double) = {
		fixedPoint( averageDamp(y=>x/y) )(1.0)
		//in this example averageDamp is simply being definied, not used,
		//it is used INSIDE fixed point, with its second argument the Double as the "guess"
	};System.out.println("""sqrt: (x: Double)Double""");$skip(13); val res$4 = 
	
	sqrt(2.0);System.out.println("""res4: Double = """ + $show(res$4))}
}
