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
    def pascal(c: Int, r: Int): Int = {
     if (c == 0 || c == r)
       1
     else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def helper(l: List[Char], countOpen: Int, countClose: Int): Boolean = {
        if (l.isEmpty) {
          if (countOpen == countClose)
            true
          else
            false
        }
        else if ((countOpen - countClose) <= 0 && l.head == ')')
          false
        else {
          if (!l.isEmpty && l.head == '(')
            helper(l.tail, countOpen + 1, countClose)
          else if (!l.isEmpty && l.head == ')')
            helper(l.tail, countOpen, countClose + 1)
          else
            helper(l.tail, countOpen, countClose)
        }
      }

       helper(chars, 0,0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

     if (coins.isEmpty || money < 0)
       0
     else if (money == 0)
       1
     else
       countChange(money-coins.head, coins) + countChange(money, coins.tail)
   }

}