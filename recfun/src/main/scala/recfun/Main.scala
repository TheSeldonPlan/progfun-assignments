package recfun
import common._
import java.util.Stack

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
    {
      if (c == 0 || r == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    {
      def innerBalance(chars: List[Char], leftParen: Integer): Boolean =
        {
          if (chars.isEmpty) {
            leftParen == 0
          } else if (chars.head == '(') {
            innerBalance(chars.tail, leftParen + 1)
          } else if (chars.head == ')') {
            leftParen > 0 && innerBalance(chars.tail, leftParen - 1)
          } else {
            innerBalance(chars.tail, leftParen)
          }
        }
      innerBalance(chars, 0)
    }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    {
      val sortedCoins = coins.sortWith(_ > _)
      val numCoins = sortedCoins.size
      def countChangeInner(money: Int, coins: List[Int]): Int =
        {
          if (money < 0) {
            0
          } else if (money == 0) {
            1
          } else if (coins.size <= 0 && money >= 1) {
            0
          } else (countChangeInner(money, coins.tail) + countChangeInner(money-coins.head, coins))
        }
        countChangeInner(money, sortedCoins)
    }
}
