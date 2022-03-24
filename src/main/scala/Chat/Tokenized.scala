package Chat

import Chat.Token.*
import Utils.SpellCheckerService

import scala.compiletime.ops.int.-

trait Tokenized:
  /**
    * Get the next token of the user input, or EOL if there is no more token.
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(val tokens: Array[(String, Token)]) extends Tokenized:
  // TODO - Part 1 Step 3
  var index:Int = -1
  def nextToken(): (String, Token) = {
    index += 1
    if index > tokens.length
    then ("EOL", Token.EOL)
    else tokens(index)
  }
end TokenizedImpl
