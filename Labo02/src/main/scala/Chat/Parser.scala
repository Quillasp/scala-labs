package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg){}

// TODO - step 4
class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an error. */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type Token */
  private def expected(token: Token, more: Token*): Nothing =
    val expectedTokens = more.prepended(token).mkString(" or ")
    throw new UnexpectedTokenException(s"Expected: $expectedTokens, found: $curToken")

  /** the root method of the parser: parses an entry phrase */
  // TODO - Part 2 Step 4
  def parsePhrases() : ExprTree =
    if curToken == BONJOUR then readToken()
    if curToken == JE then
      readToken()
      if curToken == ETRE then
        eat(ETRE)
        if curToken == ASSOIFFE then
          readToken()
          Thirsty()
        else if curToken == AFFAME then
          readToken()
          Hungry()
        else if curToken == PSEUDO then
          val p = curValue.substring(1)
          readToken()
          Pseudo(p)
        else expected(ASSOIFFE, AFFAME, PSEUDO)
      else if curToken == VOULOIR then
        eat(VOULOIR)
        eat(SOLDE)
        Balance()
      else expected(ETRE, VOULOIR)
    else expected(BONJOUR, JE)
