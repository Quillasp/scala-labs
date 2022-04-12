package Chat

import Chat.Token.*
import Utils.SpellCheckerService

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  // TODO - Part 1 Step 3
  def tokenize(input: String): Tokenized = {
    val transformed = input.replaceAll("[.,!?*]+", "") // Remove special characters
      .replaceAll("[' ]+", " ")
      .trim()
    val splitted = transformed.split(" ") // Split into separated words

    var output: Array[(String, Token)] = splitted.map(w => {
      // Get the closest word from dictionnary
      val word:String = if spellCheckerSvc.dictionary.contains(w)
          then spellCheckerSvc.dictionary.get(w).orNull
          else spellCheckerSvc.getClosestWordInDictionary(w)
      // Get the right token for the word
      val token:Token = selectToken(word)
      (word, token)
    })
    return TokenizedImpl(output)
  }

  def selectToken(word:String):Token = {
    word match {
      case "bonjour" => Token.BONJOUR
      
      case "je" => Token.JE
      case "me" => Token.ME
      case "mon" => Token.ME
      case "le" => Token.LE
      case "de" => Token.DE

      case "affame" => Token.AFFAME
      case "assoiffe" => Token.ASSOIFFE
      
      case "etre" => Token.ETRE
      case "vouloir" => Token.VOULOIR
      case "appelle" => Token.APPELLE
      case "commander" => Token.COMMANDER
      case "connaitre" => Token.CONNAITRE
      
      case "biere" => Token.PRODUCT
      case "croissant" => Token.PRODUCT
      
      case "et" => Token.ET
      case "ou" => Token.OU
      
      case "svp" => Token.SVP
      case "quel" => Token.QUEL
      case "combien" => Token.COMBIEN
      
      case "coute" => Token.COUT
      case "solde" => Token.SOLDE
      case "prix" => Token.PRIX
      
      case "maison" => Token.MARQUE
      case "cailler" => Token.MARQUE
      case "farmer" => Token.MARQUE
      case "boxer" => Token.MARQUE
      case "wittekop" => Token.MARQUE
      case "punkipa" => Token.MARQUE
      case "jackhammer" => Token.MARQUE
      case "ténébreuse" => Token.MARQUE
      
      case s if s(0) == '_' => Token.PSEUDO
      case s if (s.toIntOption).isDefined => Token.NUM
      case _ => Token.EOL
    }
  }
end TokenizerService
