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
     * calls). So we store the the input of the function helper as the key and its output as the value.
     */
    val memoizedCosts = mutable.Map[(List[Char],List[Char]),Int]()

    // function helper to compute the Levenshtein distance between 2 lists of chars
    def sd(s1: List[Char], s2: List[Char]): Int =
      // We check if the function has already computed the costs, if not
      if !memoizedCosts.contains((s1, s2)) then
      // We recursively create the memoization
        memoizedCosts((s1,s2)) = (s1, s2) match {
          case (_, Nil) => s1.length
          case (Nil, _) => s2.length
          /* If both strings are not empty, we take the head and tail of both and then compute the distance following
           * the rules
           * TODO finish documentation of the stringDistance method
           */
          case (c1::t1, c2::t2)  => List( sd(t1, s2) + 1,
                                          sd(s1, t2) + 1,
                                          sd(t1, t2) + (if(c1==c2) 0 else 1)).min
        }
      end if
      // If it's already there, we simply return it
      memoizedCosts((s1,s2))

    sd(s1.toList, s2.toList)



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
