package Chat

import Chat.ExprTree.{Balance, Hungry, Identification, Thirsty}
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

  private def eatAll(tokens: List[Token]):String =
    tokens match {
      case nil => ""
      case (head :: nil) => eat(head)
      case (head :: tail) => {
        eat(head)
        eatAll(tail)
      }
    }

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
      case COMBIEN | QUEL => parseCombienPhrases()
      case _ => expected(BONJOUR, JE, COMBIEN, QUEL)
    }

  def parseJePhrases() : ExprTree = {
    eat(JE)
    this.curToken match {
      case ETRE => parseEtrePhrases()
      case VOULOIR => parseVouloirPhrases()
      case ME => {
        eatAll(List(ME, APPELLE))
        parsePseudo()
      }
      case _ => expected(ETRE, VOULOIR)
    }
  }

  def parseCombienPhrases() : ExprTree = {
    this.curToken match {
      case COMBIEN => eatAll(List(COMBIEN, COUTE))
      case QUEL => eatAll(List(QUEL, ETRE, LE, PRIX, DE))
      case _ => expected(COMBIEN, QUEL)
    }
    val order = parseOrder()
    AskPrice(order)
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
    case CONNAITRE => parseConnaitePhrases()
    case COMMANDER => {
      eat(COMMANDER)
      parseOrder()
    }
    case NUM => parseOrder()
    case _ => expected(CONNAITRE, COMMANDER, NUM)
  }

  def parseConnaitePhrases() : ExprTree = {
    eat(CONNAITRE)
    this.curToken match {
      // je veux connaître mon solde
      case ME => {
        eatAll(List(ME, SOLDE))
        Balance()
      }
      // Je veux savoir combien coute....
      case COMBIEN | QUEL  => parseCombienPhrases()
      case _ => expected(ME, COMBIEN, QUEL)
    }
  }


  def parsePseudo(): ExprTree = {
    val p = eat(PSEUDO).substring(1)
    Identification(p)
  }

  def parseOrder(): ExprTree = {
    val nb = eat(NUM).toInt
    val productType = eat(PRODUCT)
    val brandName = if curToken == MARQUE then eat(MARQUE) else ""

    val order = Order(nb, productType, brandName)
    // Recursion for AND/OR orders
    this.curToken match {
      case ET => readToken(); And(order, parseOrder())
      case OU => readToken(); Or(order, parseOrder())
      case EOL => order
      case _ => expected(ET, OU, EOL)
    }
  }


