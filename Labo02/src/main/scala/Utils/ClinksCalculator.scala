package Utils

import scala.annotation.tailrec

/**
  * Contains the function necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator:
  /**
    * Calculate the factorial of a given number
    * @param n the number to compute
    * @return n!
    */
  // TODO - Part 1 Step 1
  def factorial(n: Int): BigInt =
    @tailrec
    def factAcc(acc: Int, n: Int): BigInt =
      if (n == 1 || n == 0)
        acc
      else
        factAcc(acc * n, n - 1)

    if n < 0
    then throw new Exception("n should be non-negative")
    else factAcc(1, n)

  /**
    * Calculate the combination of two given numbers
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  // TODO - Part 1 Step 1
  def calculateCombination(n: Int, k: Int): Int =
    if k > n then 0
    else (factorial(n) / (factorial(k) * factorial(n - k))).toInt
end ClinksCalculator
