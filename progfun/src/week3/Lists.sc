package week3

object Lists {
  def nth[T](n: Int, xs: List[T]): T =
  	if(xs.isEmpty)  throw new IndexOutOfBoundsException
  	else if(n==0) xs.head
  	else nth(n-1, xs.tail)                    //> nth: [T](n: Int, xs: week3.List[T])T
  	
  	val list = new Const(1, new Const(2, new Const(3, new Nil)))
                                                  //> list  : week3.Const[Int] = week3.Const@d1dca40
  	
  	nth(2,list)                               //> res0: Int = 3
  	nth(4,list)                               //> java.lang.IndexOutOfBoundsException
                                                  //| 	at week3.Lists$$anonfun$main$1.nth$1(week3.Lists.scala:5)
                                                  //| 	at week3.Lists$$anonfun$main$1.apply$mcV$sp(week3.Lists.scala:12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week3.Lists$.main(week3.Lists.scala:3)
                                                  //| 	at week3.Lists.main(week3.Lists.scala)
  	
  	nth(-1,list)
}