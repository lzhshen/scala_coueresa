package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if ((c == 0) || (c == r)) 1
      else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balIter(str: List[Char], count: Int): Boolean =
        if (count < 0) false
        else if ((str.isEmpty) && (count == 0)) true
        else {
          str.head match {
            case '(' => balIter(str.tail, count + 1)
            case ')' => balIter(str.tail, count - 1)
            case _ => balIter(str.tail, count)
          }
        }
      balIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeIter(money: Int, coins: List[Int]): Int =
        if (money == 0) 1
        else if ((money < 0) || (coins.isEmpty)) 0
        else countChangeIter(money - coins.head, coins) + countChangeIter(money, coins.tail)
      countChangeIter(money, coins)
    }
  }
