package week3

object Lists {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(174); 
  def nth[T](n: Int, xs: List[T]): T =
  	if(xs.isEmpty)  throw new IndexOutOfBoundsException
  	else if(n==0) xs.head
  	else nth(n-1, xs.tail);System.out.println("""nth: [T](n: Int, xs: week3.List[T])T""");$skip(68); 
  	
  	val list = new Const(1, new Const(2, new Const(3, new Nil)));System.out.println("""list  : week3.Const[Int] = """ + $show(list ));$skip(19); val res$0 = 
  	
  	nth(2,list);System.out.println("""res0: Int = """ + $show(res$0));$skip(15); val res$1 = 
  	nth(4,list);System.out.println("""res1: Int = """ + $show(res$1));$skip(20); val res$2 = 
  	
  	nth(-1,list);System.out.println("""res2: Int = """ + $show(res$2))}
}
