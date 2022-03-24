package Utils

import scala.collection.mutable

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Calculate the Levenstein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    * @param misspelledWord the mispelled word to correct
    * @return the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService:
  // TODO - Part 1 Step 2
  def stringDistance(s1: String, s2: String): Int =
    /* Used for memoization, because if not, with a functional implementation of the Levenshtein distance there would be
     * a massive number of calls depending on the string length (i.e. for 2 words of length 6, there would be 13483
     * calls). So we store the input of the function helper as the key and its output as the value.
     */
    if s1.isEmpty then return s2.length
    if s2.isEmpty then return s1.length
    val memoizedCosts = mutable.Map[(List[Char],List[Char]),Int]()
    // We store the words reversed, because we will add each letter to the front of lists
    val words = (s1.reverse.toVector, s2.reverse.toVector)
    // function helper to compute the Levenshtein distance between 2 lists of chars
    def sd(l1: List[Char], l2: List[Char]): Int =
      // We compute the Levenshtein distance of current words, based on already computed distances
      var value = -1
      if l1.isEmpty then
        // Base case
        value = l2.size
      else if l2.isEmpty then
        // Base case
        value = l1.size
      else
        val (h1::t1) = l1
        val (h2::t2) = l2
        // Is the first letter of the words the same ? (remember we construct them in reverse order)
        val sameLetter = if (h1 == h2) then 0 else 1
        // Compute the minimum
        value = List(
          memoizedCosts((t1, l2)) + 1,
          memoizedCosts((l1, t2)) + 1,
          memoizedCosts((t1, t2)) + sameLetter,
        ).min
      end if
      // Set the new value
      memoizedCosts((l1,l2)) = value
      // Is it the end ?
      if l1.length == words._1.length &&  l2.length == words._2.length then
        memoizedCosts.getOrElse((l1, l2), -1)
      else
        // Depending on the case, only l1 or l2 need to be changed
        var n_l1:List[Char] = List() // next l1 value if l2 need to be changed
        var n_l2:List[Char] = l2 // next l2 value if l1 need to be changed
        // We complete the table column by column, we check what's the next value we need to compute
        if l1.length >= words._1.length then
          // Add next letter to n_l2, n_l1 is already empty
          n_l2 = words._2(l2.length)::l2
        else
          // Add next letter to n_l1, n_l2 is still l2
          n_l1 = words._1(l1.length)::l1
        end if
        // Tail recursive call for next computation
        sd(n_l1, n_l2)
      end if

    // Base call : two empty words
    sd(Nil, Nil)



  // TODO - Part 1 Step 2
  def getClosestWordInDictionary(misspelledWord: String): String =
    misspelledWord match {
      // If the string start with a digit or with an underscore, we just return it
      case s if s(0) == '_' || s(0).isDigit => misspelledWord
      // If not we compute the distance with each key and take the min and return the associated value
      case s => dictionary.get(
        dictionary
        .keySet // Getting a set of all keys
        .toList // Cast to List
        .map(w => (w, stringDistance(w, misspelledWord))) // Create a tuple with (word, distance)
        .minBy(t => t._2) // Getting the min with the distance as the ordering parameter
        ._1) match {
        case Some(s) => s // The Map's get method return an Option, so we return the value that's in it
      }
    }
end SpellCheckerImpl
