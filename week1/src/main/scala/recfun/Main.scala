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
		if (c == 0 || c == r) 1
		else {
		  if (c>r) -1
		  else pascal(c - 1, r - 1) + pascal(c, r - 1)
		}
	  }

  /**
    * Exercise 2.
    */
	  def balance(chars: List[Char]) = {

		def Which(c: Char): Int = if (c == '(') 1 else -1

		def IsBraket(c: Char) = c == '(' || c == ')'

		def CheckAcc(acc: Int): Boolean = (acc) == 0

		def IfBraket(c: Char): Int = if (IsBraket(c)) Which(c) else 0

		def Iter(chars: List[Char], acc: Int): Boolean =
		  if (chars.isEmpty || acc < 0) CheckAcc(acc)
		  else Iter(chars.tail, acc + IfBraket(chars.head))

		Iter(chars, 0)
	  }

  /**
    * Exercise 3
    */

	  def countChange(money: Int, coins: List[Int]): Int = {
		def Strip(money: Int, coins: List[Int]): List[Int] =
		  for (i <- List.range(0, coins.size) if coins(i)<=money) yield coins(i)
		if (Strip(money, coins).isEmpty && money==0) 1
		else if (Strip(money, coins).isEmpty && money!=0) 0
		//    else if (money - coins.head < 0) 0
		//    else if (money - coins.head == 0) 1
		//    else countSingle(money - coins.head, coins) + countSingle(money, coins.tail)
		else countChange(money - coins.head, coins) + countChange(money, coins.tail)
	  }
}
