package Utils

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
  def factorial(n: Int): Int =
    def factAcc(acc: Int, n: Int): Int =
      if (n == 1 || n == 0)
        return acc
      else
        factAcc(acc * n, n - 1)

    factAcc(1, n)

  /**
    * Calculate the combination of two given numbers
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  // TODO - Part 1 Step 1
  def calculateCombination(n: Int, k: Int): Int =
    factorial(n) / (factorial(k) * factorial(n - k))
end ClinksCalculator
