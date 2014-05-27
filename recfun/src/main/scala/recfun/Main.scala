package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 16) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == r || c == 0){
      1
    }else{
      pascal(c-1,r-1) + pascal(c,r-1)
    }
  }

  /**
   * Exercise 2
   */
  //create a variable that defines a string and then use toList method
  def balance(chars: List[Char]): Boolean = {
    def balance(countThemAll: Int, chars:List[Char]) : Boolean = {
      if(countThemAll < 0 ) {
        false
      }else if(countThemAll == 0 && chars.isEmpty){
        true
      }else if(chars.head == '('){
        balance(countThemAll + 1,chars.tail)
      }else if(chars.head == ')'){
        balance(countThemAll - 1, chars.tail)
      }else{
        balance(countThemAll, chars.tail)
      }
    }
    if(chars.isEmpty){
      true
    }else{
      balance(0, chars)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty){
      0
    }else if(money == 0){
      1
    }else if(money < 0){
      0
    }else{
      countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
  
}
