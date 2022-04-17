package Chat

import Chat.ExprTree.{Balance, Hungry, Pseudo, Thirsty}
import Chat.Token.{AFFAME, ASSOIFFE, CONNAITRE, ETRE, ME, PSEUDO, SOLDE, VOULOIR}

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
    this.curToken match {
      case JE => parseJePhrases()
      case COMBIEN => {
        eat(COMBIEN)
        eat(COUTE)
        val order = parseOrder()
        AskPrice(order)
      }
      case QUEL => {
        eat(QUEL)
        eat(ETRE)
        eat(LE)
        eat(PRIX)
        eat(DE)
        val order = parseOrder()
        AskPrice(order)
      }
      case _ => expected(BONJOUR, JE, COMBIEN, QUEL)
    }


//TODO faire des functions qui parse des phrases spécifiques
  def parseJePhrases() : ExprTree = {
    eat(JE)
    this.curToken match {
      case ETRE => parseEtrePhrases()
      case VOULOIR => parseVouloirPhrases()
      case ME => {
        eat(ME)
        eat(APPELLE)
        parsePseudo()
      }
      case _ => expected(ETRE, VOULOIR)
    }
  }

  def parseEtrePhrases(): ExprTree =
    eat(ETRE)
    this.curToken match {
      case ASSOIFFE => {
        readToken()
        Thirsty()
      }
      case AFFAME => {
        readToken()
        Hungry()
      }
      case PSEUDO => parsePseudo()
      case _ => expected(ASSOIFFE, AFFAME, PSEUDO)
  }

  def parseVouloirPhrases(): ExprTree =
    eat(VOULOIR)
    this.curToken match {
      case CONNAITRE => {
        eat(CONNAITRE)
        eat(ME)
        eat(SOLDE)
        Balance()
      }
    case NUM => parseOrder()
    case _ => expected(CONNAITRE)
  }

  def parsePseudo(): ExprTree.Pseudo = {
    val p = curValue.substring(1)
    readToken()
    Pseudo(p)
  }

  def parseOrder(): ExprTree = {
    val nb = curValue.toInt
    eat(NUM)
    val productType = curValue
    eat(PRODUCT)
    val brandName = if curToken == MARQUE then curValue else ""
    if curToken == MARQUE then eat(MARQUE)
    val order = Order(nb, productType, brandName)
    this.curToken match {
      case ET => readToken(); And(order, parseOrder())
      case OU => readToken(); Or(order, parseOrder())
      case EOL => order
      case _ => expected(ET, OU, EOL)
    }
  }


