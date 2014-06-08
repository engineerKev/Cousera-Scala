package week5
import math.Ordering
object Lists {
  //println("Welcome to the Scala worksheet")
  val fruit = List("apples", "oranges", "pears")  //> fruit  : List[String] = List(apples, oranges, pears)
  val nums = List(1, 2, -1, 3,-4,-5, 1035)        //> nums  : List[Int] = List(1, 2, -1, 3, -4, -5, 1035)
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
                                                  //> diag3  : List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
                                                  //| 
  nums filter (x => x > 0)                        //> res0: List[Int] = List(1, 2, 3, 1035)
  nums filterNot(x => x > 0)                      //> res1: List[Int] = List(-1, -4, -5)
  nums partition (x => x > 0)                     //> res2: (List[Int], List[Int]) = (List(1, 2, 3, 1035),List(-1, -4, -5))
  nums takeWhile(x => x > 0)                      //> res3: List[Int] = List(1, 2)
  //^ it will filter list up until it finds an element that does not match the predicate
  nums dropWhile(x => x > 0)                      //> res4: List[Int] = List(-1, 3, -4, -5, 1035)
  nums span (x => x > 0)                          //> res5: (List[Int], List[Int]) = (List(1, 2),List(-1, 3, -4, -5, 1035))
  //^splits list until it an element does not match the predicate, then it puts the the predicate and the rest into its own list

  def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1) //take everything UP UNTIL n, then DROP EVERYTHING except all that come AFTER your index
                                                  //> removeAt: (n: Int, xs: List[Int])List[Int]
  //n starts at 0 TAKE and DROP use counting numbers

  removeAt(1, nums)                               //> res6: List[Int] = List(1, -1, 3, -4, -5, 1035)

  nums drop 1                                     //> res7: List[Int] = List(2, -1, 3, -4, -5, 1035)

  //MERGE SORT

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: (xs: List[Int])List[Int]

  val nums2 = List(2, -4, 5, 7, 1)                //> nums2  : List[Int] = List(2, -4, 5, 7, 1)
  msort(nums2)                                    //> res8: List[Int] = List(-4, 1, 2, 5, 7)

  //IMPLICIT PARAMETERS

  def mergesort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merg3(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x, y)) x :: merg3(xs1, ys)
          else y :: merg3(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merg3(mergesort(fst)(lt), mergesort(snd)(lt))
    }
  }                                               //> mergesort: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]

  val lenums = List(2, -4, 5, 7, 1)               //> lenums  : List[Int] = List(2, -4, 5, 7, 1)
  mergesort(nums2)((x: Int, y: Int) => x < y)     //> res9: List[Int] = List(-4, 1, 2, 5, 7)
  mergesort(fruit)((x: String, y: String) => x.compareTo(y) < 0)
                                                  //> res10: List[String] = List(apples, oranges, pears)

  //MERGESORT WITH ORDERING
  def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def m3rg3(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: m3rg3(xs1, ys)
          else y :: m3rg3(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      m3rg3(mergeSort(fst), mergeSort(snd))
    }
  }                                               //> mergeSort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  val leNums = List(2, -4, 5, 7, 1)               //> leNums  : List[Int] = List(2, -4, 5, 7, 1)
  mergeSort(nums2)                                //> res11: List[Int] = List(-4, 1, 2, 5, 7)
  mergeSort(fruit)                                //> res12: List[String] = List(apples, oranges, pears)
  
  //USING MAP
  
  def squareList(xs:List[Int]): List[Int] = xs match {
  	case Nil => Nil
  	case y :: ys => y*y :: squareList(ys)
  }                                               //> squareList: (xs: List[Int])List[Int]
  
  def squareListMap(xs:List[Int]): List[Int] = xs map (e => e*e)
                                                  //> squareListMap: (xs: List[Int])List[Int]
  
  //HIGH ORDER FUNCTIONS AND LISTS
  val packData = List("a","a","a","b","c","c","a")//> packData  : List[String] = List(a, a, a, b, c, c, a)
  def pack[T](xs: List[T]): List[List[T]] = xs match {
  	case Nil => Nil
  	case x :: xs1 =>
  		val (first, rest) = xs span (y => y == x)
  		first :: pack(rest)
  		
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  def encode[T](es: List[T]): List[(T, Int)] = {
  	pack(es) map (ys => (ys.head, ys.length))
  }                                               //> encode: [T](es: List[T])List[(T, Int)]
  pack(packData)                                  //> res13: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a
                                                  //| ))
  encode(packData)                                //> res14: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
}