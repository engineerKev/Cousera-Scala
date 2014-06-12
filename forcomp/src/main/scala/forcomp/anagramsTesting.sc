package forcomp

object anagramsTesting {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.anagramsTesting.Word] = List(Aarhus, Aaron, Ababa
                                                  //| , aback, abaft, abandon, abandoned, abandoning, abandonment, abandons, abase
                                                  //| , abased, abasement, abasements, abases, abash, abashed, abashes, abashing, 
                                                  //| abasing, abate, abated, abatement, abatements, abater, abates, abating, Abba
                                                  //| , abbe, abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbre
                                                  //| viates, abbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, 
                                                  //| abdominal, abduct, abducted, abduction, abductions, abductor, abductors, abd
                                                  //| ucts, Abe, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aber
                                                  //| ration, aberrations, abet, abets, abetted, abetter, abetting, abeyance, abho
                                                  //| r, abhorred, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, 
                                                  //| abiding, Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, a
                                                  //| bjections, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate,
                                                  //|  ablated, ablates, ablat
                                                  //| Output exceeds cutoff limit.

  def wordOccurrences(w: Word): Occurrences = (for ((x, y) <- (w.toLowerCase.groupBy(c => c)).toList) yield (x, y.length)) sortBy (a => a._1)
                                                  //> wordOccurrences: (w: forcomp.anagramsTesting.Word)forcomp.anagramsTesting.Oc
                                                  //| currences

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString.toLowerCase.filter(w => w.isLetter))
                                                  //> sentenceOccurrences: (s: forcomp.anagramsTesting.Sentence)forcomp.anagramsTe
                                                  //| sting.Occurrences

  val eat = wordOccurrences("eat")                //> eat  : forcomp.anagramsTesting.Occurrences = List((a,1), (e,1), (t,1))

  val tea = wordOccurrences("tea")                //> tea  : forcomp.anagramsTesting.Occurrences = List((a,1), (e,1), (t,1))

  val ate = wordOccurrences("ate")                //> ate  : forcomp.anagramsTesting.Occurrences = List((a,1), (e,1), (t,1))

  val dic: List[Word] = List("eat", "tea", "ate") //> dic  : List[forcomp.anagramsTesting.Word] = List(eat, tea, ate)

  val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dic groupBy (w => wordOccurrences(w))
                                                  //> dictionaryByOccurrences  : Map[forcomp.anagramsTesting.Occurrences,List[forc
                                                  //| omp.anagramsTesting.Word]] = Map(List((a,1), (e,1), (t,1)) -> List(eat, tea,
                                                  //|  ate))

  def wordAnagrams(word: Word): List[Word] = ???  //> wordAnagrams: (word: forcomp.anagramsTesting.Word)List[forcomp.anagramsTesti
                                                  //| ng.Word]

  (dictionaryByOccurrences get wordOccurrences("tea")).head
                                                  //> res0: List[forcomp.anagramsTesting.Word] = List(eat, tea, ate)

  val combinationtest = List(('a', 2), ('b', 2))  //> combinationtest  : List[(Char, Int)] = List((a,2), (b,2))

  combinationtest map (x => x._2)                 //> res1: List[Int] = List(2, 2)

  combinationtest map (pair => (pair._1, (1 to pair._2)))
                                                  //> res2: List[(Char, scala.collection.immutable.Range.Inclusive)] = List((a,Ra
                                                  //| nge(1, 2)), (b,Range(1, 2)))

  //val lepairs = ((1 until n) map (i => (1 until i) map (j => (i, j))))

  combinationtest map (pair => (1 to pair._2) map (i => (pair._1, i)))
                                                  //> res3: List[scala.collection.immutable.IndexedSeq[(Char, Int)]] = List(Vecto
                                                  //| r((a,1), (a,2)), Vector((b,1), (b,2)))

  combinationtest groupBy (pair => (1 to pair._2))//> res4: scala.collection.immutable.Map[scala.collection.immutable.Range.Inclu
                                                  //| sive,List[(Char, Int)]] = Map(Range(1, 2) -> List((a,2), (b,2)))

  combinationtest map (x => x._1) zip (for ((x, y) <- combinationtest) yield (1 to y).toList)
                                                  //> res5: List[(Char, List[Int])] = List((a,List(1, 2)), (b,List(1, 2)))

  combinationtest map (x => (1 to x._2))          //> res6: List[scala.collection.immutable.Range.Inclusive] = List(Range(1, 2), 
                                                  //| Range(1, 2))

  val letters = (for ((x, y) <- combinationtest) yield (x)).toSet
                                                  //> letters  : scala.collection.immutable.Set[Char] = Set(a, b)

  val singles = for {
    (x, y) <- combinationtest
    j <- 1 to y
  } yield List(x, j)                              //> singles  : List[List[Int]] = List(List(97, 1), List(97, 2), List(98, 1), Li
                                                  //| st(98, 2))

  val attempts = for {
    (x, y) <- combinationtest
    j <- 1 to y
  } yield (j)                                     //> attempts  : List[Int] = List(1, 2, 1, 2)

  val nums = attempts.toSet                       //> nums  : scala.collection.immutable.Set[Int] = Set(1, 2)

  val numsets = nums.subsets.map(_.toList).toList //> numsets  : List[List[Int]] = List(List(), List(1), List(2), List(1, 2))

  val numsfull = numsets filter (x => !x.isEmpty) //> numsfull  : List[List[Int]] = List(List(1), List(2), List(1, 2))

  val lettersets = letters.subsets.map(_.toList).toList
                                                  //> lettersets  : List[List[Char]] = List(List(), List(a), List(b), List(a, b))
                                                  //| 

  val lettersfull = lettersets filter (x => !x.isEmpty)
                                                  //> lettersfull  : List[List[Char]] = List(List(a), List(b), List(a, b))

  val doubles = lettersets map (i => numsets flatMap (j => i zip j))
                                                  //> doubles  : List[List[(Char, Int)]] = List(List(), List((a,1), (a,2), (a,1))
                                                  //| , List((b,1), (b,2), (b,1)), List((a,1), (a,2), (a,1), (b,2)))

  val singles2 = (for (i <- 1 to combinationtest.head._2) yield (combinationtest.head._1, i)).toList
                                                  //> singles2  : List[(Char, Int)] = List((a,1), (a,2))

  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(Nil)
    case (c, n) :: others =>
      val tails = combinations(others)
      tails ::: (for {
        j <- tails
        i <- 1 to n
      } yield (c, i) :: j)
  }                                               //> combinations: (occurrences: forcomp.anagramsTesting.Occurrences)List[forcom
                                                  //| p.anagramsTesting.Occurrences]

  combinations(combinationtest)                   //> res7: List[forcomp.anagramsTesting.Occurrences] = List(List(), List((b,1)),
                                                  //|  List((b,2)), List((a,1)), List((a,2)), List((a,1), (b,1)), List((a,2), (b,
                                                  //| 1)), List((a,1), (b,2)), List((a,2), (b,2)))

  1 until 2                                       //> res8: scala.collection.immutable.Range = Range(1)

val mySentence:List[Word] = List("today", "I", "went", "to", "the", "festival")
                                                  //> mySentence  : List[forcomp.anagramsTesting.Word] = List(today, I, went, to,
                                                  //|  the, festival)

val myWords: List[Word] = List("today", "to", "went")
                                                  //> myWords  : List[forcomp.anagramsTesting.Word] = List(today, to, went)

val subSet = sentenceOccurrences(myWords).sortBy(x=>x._1)
                                                  //> subSet  : List[(Char, Int)] = List((a,1), (d,1), (e,1), (n,1), (o,2), (t,3)
                                                  //| , (w,1), (y,1))


val unwanted = subSet.toSet                       //> unwanted  : scala.collection.immutable.Set[(Char, Int)] = Set((e,1), (w,1),
                                                  //|  (n,1), (d,1), (t,3), (a,1), (o,2), (y,1))
val superSet = sentenceOccurrences(mySentence).sortBy(x=>x._1)
                                                  //> superSet  : List[(Char, Int)] = List((a,2), (d,1), (e,3), (f,1), (h,1), (i,
                                                  //| 2), (l,1), (n,1), (o,2), (s,1), (t,5), (v,1), (w,1), (y,1))

val superS = superSet.toSet                       //> superS  : scala.collection.immutable.Set[(Char, Int)] = Set((i,2), (w,1), (
                                                  //| a,2), (s,1), (h,1), (e,3), (n,1), (l,1), (d,1), (v,1), (f,1), (t,5), (o,2),
                                                  //|  (y,1))

superSet filterNot(unwanted) groupBy(x=>x._1)     //> res9: scala.collection.immutable.Map[Char,List[(Char, Int)]] = Map(e -> Lis
                                                  //| t((e,3)), s -> List((s,1)), t -> List((t,5)), f -> List((f,1)), a -> List((
                                                  //| a,2)), i -> List((i,2)), v -> List((v,1)), l -> List((l,1)), h -> List((h,1
                                                  //| )))

subSet filterNot(superS)                          //> res10: List[(Char, Int)] = List((a,1), (e,1), (t,3))

  
  
def subtract(superSet: Occurrences, subSet: Occurrences): Occurrences = {
	def uniqueSet(outer:Occurrences, inner:Occurrences, acc:Occurrences): Occurrences = (inner, outer) match {
		case (List(),_) => acc ::: outer
		case ((c_i,n_i) :: others_i, (c_o, n_o) :: others_o) =>
			if(c_i == c_o && (n_i < n_o)) uniqueSet(others_o, others_i,(c_o,n_o-n_i) :: acc)
			else uniqueSet(others_o, inner, (c_o,n_o)::acc )
	}
	uniqueSet(superSet filterNot(subSet.toSet), subSet filterNot(superSet.toSet), List()).sortBy(x=>x._1)
}                                                 //> subtract: (superSet: forcomp.anagramsTesting.Occurrences, subSet: forcomp.a
                                                  //| nagramsTesting.Occurrences)forcomp.anagramsTesting.Occurrences

subtract(superSet, subSet)                        //> res11: forcomp.anagramsTesting.Occurrences = List((a,1), (e,2), (f,1), (h,1
                                                  //| ), (i,2), (l,1), (s,1), (t,2), (v,1))

  /*
   * occurrence list `List(('a', 2), ('b', 2))`
   *List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   */

  //val numsSubset = nums.subsets.map(_.toList).toList

}