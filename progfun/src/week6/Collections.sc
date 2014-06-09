package week6

import scala.io.Source

object Collections {
  //println("Welcome to the Scala worksheet")

  val xs = Array(1, 2, 3, 44)                     //> xs  : Array[Int] = Array(1, 2, 3, 44)
  xs map (x => x * 2)                             //> res0: Array[Int] = Array(2, 4, 6, 88)

  val s = "Hello World"                           //> s  : String = Hello World
  s filter (c => c.isUpper)                       //> res1: String = HW

  val r: Range = 1 until 5                        //> r  : Range = Range(1, 2, 3, 4)
  val t: Range = 1 to 5                           //> t  : Range = Range(1, 2, 3, 4, 5)
  1 to 10 by 3                                    //> res2: scala.collection.immutable.Range = Range(1, 4, 7, 10)
  6 to 1 by -2                                    //> res3: scala.collection.immutable.Range = Range(6, 4, 2)

  s exists (c => c.isUpper)                       //> res4: Boolean = true
  s forall (c => c.isUpper)                       //> res5: Boolean = false

  val pairs = List(1, 2, 3) zip s                 //> pairs  : List[(Int, Char)] = List((1,H), (2,e), (3,l))

  pairs.unzip                                     //> res6: (List[Int], List[Char]) = (List(1, 2, 3),List(H, e, l))

  s flatMap (c => List('.', c))                   //> res7: String = .H.e.l.l.o. .W.o.r.l.d

  xs.sum                                          //> res8: Int = 50
  xs.max                                          //> res9: Int = 44

  (1 to 10) flatMap (x => (1 to 5) map (y => (x, y)))
                                                  //> res10: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,
                                                  //| 2), (1,3), (1,4), (1,5), (2,1), (2,2), (2,3), (2,4), (2,5), (3,1), (3,2), (3
                                                  //| ,3), (3,4), (3,5), (4,1), (4,2), (4,3), (4,4), (4,5), (5,1), (5,2), (5,3), (
                                                  //| 5,4), (5,5), (6,1), (6,2), (6,3), (6,4), (6,5), (7,1), (7,2), (7,3), (7,4), 
                                                  //| (7,5), (8,1), (8,2), (8,3), (8,4), (8,5), (9,1), (9,2), (9,3), (9,4), (9,5),
                                                  //|  (10,1), (10,2), (10,3), (10,4), (10,5))

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum      //> scalarProduct: (xs: Vector[Double], ys: Vector[Double])Double

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map { case (x, y) => x * y }.sum  //> scalarProduct2: (xs: Vector[Double], ys: Vector[Double])Double

  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
                                                  //> isPrime: (n: Int)Boolean

  //Nested Sequences

  val n = 7                                       //> n  : Int = 7
  val lepairs = ((1 until n) map (i => (1 until i) map (j => (i, j))))
                                                  //> lepairs  : scala.collection.immutable.IndexedSeq[scala.collection.immutable.
                                                  //| IndexedSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2
                                                  //| )), Vector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector(
                                                  //| (6,1), (6,2), (6,3), (6,4), (6,5)))
  lepairs.flatten                                 //> res11: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 1), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6
                                                  //| ,3), (6,4), (6,5))

  ((1 until n) flatMap (i => (1 until i) map (j => (i, j))))
                                                  //> res12: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3
                                                  //| ,1), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), 
                                                  //| (6,3), (6,4), (6,5))

  lepairs.flatten.filter(pair => isPrime(pair._1 + pair._2))
                                                  //> res13: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3
                                                  //| ,2), (4,1), (4,3), (5,2), (6,1), (6,5))

  //FOR-EXPRESSION EXAMPLE

  case class Person(name: String, age: Int)

  val persons: List[Person] = List(new Person("Betty", 54))
                                                  //> persons  : List[week6.Collections.Person] = List(Person(Betty,54))

  for (p <- persons if p.age > 20) yield p.name   //> res14: List[String] = List(Betty)

  def scalarProductUsingFor(xs: List[Double], ys: List[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum   //> scalarProductUsingFor: (xs: List[Double], ys: List[Double])Double

  //N-QUEENS

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
  (queens(4) map show) mkString "\n"              //> res15: String = "
                                                  //| * * X * 
                                                  //| X * * * 
                                                  //| * * * X 
                                                  //| * X * * 
                                                  //| 
                                                  //| * X * * 
                                                  //| * * * X 
                                                  //| X * * * 
                                                  //| * * X * "

  //QUERIES WITH FOR

  case class Book(title: String, authors: List[String])

  val books: List[Book] =
    List(Book(title = "Structure and Interpretation of Computer Programs", authors = List("Alberson, Harald", "Sussman, Gerald J.")),
      Book(title = "Introduction to Functional Programming", authors = List("Bird, Richard", "Wadler, Phil")),
      Book(title = " Effective Java", authors = List("Bloch, Joshua")),
      Book(title = "Java Puzzlers", authors = List("Bloch, Joshua", "Gafter, Neal")),
      Book(title = "Prigramming in Scala", authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
                                                  //> books  : List[week6.Collections.Book] = List(Book(Structure and Interpretat
                                                  //| ion of Computer Programs,List(Alberson, Harald, Sussman, Gerald J.)), Book(
                                                  //| Introduction to Functional Programming,List(Bird, Richard, Wadler, Phil)), 
                                                  //| Book( Effective Java,List(Bloch, Joshua)), Book(Java Puzzlers,List(Bloch, J
                                                  //| oshua, Gafter, Neal)), Book(Prigramming in Scala,List(Odersky, Martin, Spoo
                                                  //| n, Lex, Venners, Bill)))

  for (b <- books; a <- b.authors if a startsWith "Bloch, ") yield b.title
                                                  //> res16: List[String] = List(" Effective Java", Java Puzzlers)
  for {
    b1 <- books
    b2 <- books
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res17: List[String] = List(Bloch, Joshua, Bloch, Joshua)

  /*( for {
	b1 <- books
	b2 <- books
	if b1.title < b2.title
	a1 <- b1.authors
	a2 <- b2.authors
	if a1 == a2
 } yield a1
).distinct*/ //only for when using a list instead of a set

  //Using a set as a collection yields NO DUPLICATES

  val bookSet: Set[Book] =
    Set(Book(title = "Structure and Interpretation of Computer Programs", authors = List("Alberson, Harald", "Sussman, Gerald J.")),
      Book(title = "Introduction to Functional Programming", authors = List("Bird, Richard", "Wadler, Phil")),
      Book(title = " Effective Java", authors = List("Bloch, Joshua")),
      Book(title = "Java Puzzlers", authors = List("Bloch, Joshua", "Gafter, Neal")),
      Book(title = "Prigramming in Scala", authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
                                                  //> bookSet  : Set[week6.Collections.Book] = Set(Book(Prigramming in Scala,List
                                                  //| (Odersky, Martin, Spoon, Lex, Venners, Bill)), Book(Introduction to Functio
                                                  //| nal Programming,List(Bird, Richard, Wadler, Phil)), Book(Structure and Inte
                                                  //| rpretation of Computer Programs,List(Alberson, Harald, Sussman, Gerald J.))
                                                  //| , Book( Effective Java,List(Bloch, Joshua)), Book(Java Puzzlers,List(Bloch,
                                                  //|  Joshua, Gafter, Neal)))

  for {
    b1 <- bookSet
    b2 <- bookSet
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res18: scala.collection.immutable.Set[String] = Set(Bloch, Joshua)

  //FOR QUERY THROUGH HIGH ORDER FUNCTIONS

  //for (b <- books; a <- b.authors if a startsWith "Bloch, ") yield b.title

  books.flatMap(b => b.authors.withFilter(a => a startsWith ("Bloch")).map(y => b.title))
                                                  //> res19: List[String] = List(" Effective Java", Java Puzzlers)

  //MAPS

  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
                                                  //> romanNumerals  : scala.collection.immutable.Map[String,Int] = Map(I -> 1, V
                                                  //|  -> 5, X -> 10)

  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")
                                                  //> capitalOfCountry  : scala.collection.immutable.Map[String,String] = Map(US 
                                                  //| -> Washington, Switzerland -> Bern)

  //POLYNOMIALS

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue (0.0)
    //def + (other:Poly) = new Poly(terms ++ (other.terms map adjust))

    //using foldleft for +
    def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (expo, coeff) = term
      terms + (expo -> (coeff + terms(expo)))
    }
    //before foldLeft
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (expo, coeff) = term
      expo -> (coeff + terms(expo))
      /*terms get expo match {
			case Some(coeff1) => expo -> (coeff + coeff1)
			case None => expo -> coeff
		}*/
    }
    override def toString = (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  //val p1 = new Poly(Map(1->2.0, 3->4.0, 5 -> 6.2))<-without the new constructor
  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2) //> p1  : week6.Collections.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)           //> p2  : week6.Collections.Poly = 7.0x^3 + 3.0x^0
  p1 + p2                                         //> res20: week6.Collections.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0
  p1.terms(7)                                     //> res21: Double = 0.0

  //Putting the Pieces Togther

  val dictFilePath = "file:///Users/keruiz13/eclipse/Cousera-Scala/progfun/src/week6/linuxwords.txt"
                                                  //> dictFilePath  : String = file:///Users/keruiz13/eclipse/Cousera-Scala/progf
                                                  //| un/src/week6/linuxwords.txt
  val in = Source.fromURL(dictFilePath)           //> in  : scala.io.BufferedSource = non-empty iterator
  
  //val in3 = Source.fromURL(getClass.getResource(dictFilePath))

  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, a
                                                  //| bandoned, abandoning, abandonment, abandons, abase, abased, abasement, abas
                                                  //| ements, abases, abash, abashed, abashes, abashing, abasing, abate, abated, 
                                                  //| abatement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, 
                                                  //| abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, 
                                                  //| abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, ab
                                                  //| ducted, abduction, abductions, abductor, abductors, abducts, Abe, abed, Abe
                                                  //| l, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations
                                                  //| , abet, abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhor
                                                  //| rent, abhorrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan,
                                                  //|  Abigail, Abilene, abilities, ability, abject, abjection, abjections, abjec
                                                  //| tly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated, ablat
                                                  //| es, ablating, ablation,
                                                  //| Output exceeds cutoff limit.

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GH
                                                  //| I, 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)

  /**Invert the mnem map to give a map from chars 'A' .... 'Z' to '2' .... '9' */
  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J 
                                                  //| -> 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 
                                                  //| 5, B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, 
                                                  //| Z -> 9, S -> 7)

  /**Maps a word to the digit string it can represent, e.g. "Java" -> "5282" */
  def wordCode(word: String): String = word.toUpperCase map charCode
                                                  //> wordCode: (word: String)String

  wordCode("JAVA")                                //> res22: String = 5282
  wordCode("Java")                                //> res23: String = 5282

  /**
   * A map from digit strings to the words that represent them,
   * e,g, "5282" -> List("Java", "Kata", "Lava", ...)
   * Note: A missing number should map to the empty set, e.g. "111" -> List()
   */
  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()
                                                  //> wordsForNum  : Map[String,Seq[String]] = Map(63972278 -> List(newscast), 29
                                                  //| 237638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> Li
                                                  //| st(allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 86
                                                  //| 8437 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 3
                                                  //| 364646489 -> List(femininity), 3987267346279 -> List(extraordinary), 785539
                                                  //| 7 -> List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 3
                                                  //| 86583 -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 
                                                  //| 847827 -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curl
                                                  //| icue), 84863372658 -> List(thunderbolt), 46767833 -> List(imported), 264374
                                                  //| 64 -> List(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(
                                                  //| spoolers), 46636233 -> List(homemade), 7446768759 -> List(rigorously), 7464
                                                  //| 4647 -> List(ringings), 633738 -> List(offset), 847825 -> List(visual), 772
                                                  //| 832 -> List(Pravda), 47
                                                  //| Output exceeds cutoff limit.

  /**Return all ways to encode a number as a list of word */
  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet                                       //> encode: (number: String)Set[List[String]]

  encode("7225247386")                            //> res24: Set[List[String]] = Set(List(rack, ah, re, to), List(sack, ah, re, t
                                                  //| o), List(Scala, ire, to), List(sack, air, fun), List(rack, air, fun), List(
                                                  //| rack, bird, to), List(pack, air, fun), List(pack, ah, re, to), List(pack, b
                                                  //| ird, to), List(Scala, is, fun), List(sack, bird, to))

  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")           //> translate: (number: String)Set[String]

  translate("7225247386")                         //> res25: Set[String] = Set(sack air fun, pack ah re to, pack bird to, Scala i
                                                  //| re to, Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird t
                                                  //| o, sack ah re to, rack air fun)

}