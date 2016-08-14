package recfun

object Main {
  def main(args: Array[String]) {
    println(countChangeDP(10, List.range(1,11)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {

      def pascal (prevRow:List[Int], currentRowIndex:Int): Int =
        if (currentRowIndex == r) prevRow(c)
        else pascal(formRow(prevRow), currentRowIndex + 1)
    
      def formRow (prevRow:List[Int]): List[Int] =
        if (prevRow.size == 1) List(1, 1)
        else 1 :: prevRow.sliding(2).toList.map {xs => xs(0) + xs(1)} ::: List(1)
  
      if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException("Required: 0 <= c <= r")
      else pascal(List(1), 0)
    }
    
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      
      def balance(remaining:List[Char], left:Int, right:Int): Boolean =
        if (remaining.isEmpty) left == right
        else
          if (right > left) false
          else balance(
            remaining.tail,
            if (remaining.head == '(') left + 1 else left,
            if (remaining.head == ')') right + 1 else right
          )
          
      balance(chars, 0, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      
      def count(total:Int, remaining:List[Int]): Int =
        if (total == 0) 1
        else
          if ((remaining.isEmpty && total > 0) || total < 0) 0
          else count(total - remaining.head, remaining) + count(total, remaining.tail)
          
        count(money, coins)
    }
    
    def countChangeDP(money: Int, coins: List[Int]): Int = {
      var dp = Array.ofDim[Int](money + 1, coins.length + 1)
      for (i <- 0 until coins.size + 1) dp(0)(i) = 1
      
      for (i <- 1 until money + 1)
        for (j <- 1 until coins.size + 1)
          dp(i)(j) = if (i < coins(j - 1)) dp(i)(j - 1) else dp(i - coins(j - 1))(j) + dp(i)(j - 1)
          
      for (i <- 0 until money + 1) {
        for (j <- 0 until coins.size + 1)
      print(dp(i)(j) + " ")
      println()
      }
          dp(money)(coins.size)
    }
    
  }
