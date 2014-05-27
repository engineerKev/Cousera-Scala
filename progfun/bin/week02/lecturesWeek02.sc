package week02

object lecturesWeek02 {
  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  sum(x => x * x, 3, 5)                           //> res0: Int = 50

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
    
  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x => x * x)(3, 4)                       //> res1: Int = 144

  def factorial(n: Int) = product(x => x)(1, n)   //> factorial: (n: Int)Int
  factorial(5)                                    //> res2: Int = 120
  
  import math.abs
  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4
  def isCloseEnough(x:Double, y: Double) =
  	abs((x-y) / x) / x < tolerance            //> isCloseEnough: (x: Double, y: Double)Boolean
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  	def iterate(guess:Double): Double = {
  		println( "calling f with " + guess )
  		val next = f(guess)
  		if (isCloseEnough(guess, next)) next
  		else iterate(next)
  	}
  	iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
	fixedPoint(x=> 1 + x/2)(1)                //> calling f with 1.0
                                                  //| calling f with 1.5
                                                  //| calling f with 1.75
                                                  //| calling f with 1.875
                                                  //| calling f with 1.9375
                                                  //| calling f with 1.96875
                                                  //| calling f with 1.984375
                                                  //| calling f with 1.9921875
                                                  //| calling f with 1.99609375
                                                  //| calling f with 1.998046875
                                                  //| calling f with 1.9990234375
                                                  //| calling f with 1.99951171875
                                                  //| res3: Double = 1.999755859375
	
	def averageDamp(f: Double => Double)(x:Double) = {
	  println( "x = " + x )
	  (x+f(x))/2
  }                                               //> averageDamp: (f: Double => Double)(x: Double)Double
      
  def ad( arg: Double ) : Double = { arg }        //> ad: (arg: Double)Double
  
  println( averageDamp(y => 3/y)( 3.3) )          //> x = 3.3
                                                  //| 2.1045454545454545
                                                             
	def sqrt(x:Double) = {
		fixedPoint( averageDamp(y=>x/y) )(1.0)
		//in this example averageDamp is simply being definied, not used,
		//it is used INSIDE fixed point, with its second argument the Double as the "guess"
	}                                         //> sqrt: (x: Double)Double
	
	sqrt(2.0)                                 //> calling f with 1.0
                                                  //| x = 1.0
                                                  //| calling f with 1.5
                                                  //| x = 1.5
                                                  //| calling f with 1.4166666666666665
                                                  //| x = 1.4166666666666665
                                                  //| calling f with 1.4142156862745097
                                                  //| x = 1.4142156862745097
                                                  //| res4: Double = 1.4142135623746899
}