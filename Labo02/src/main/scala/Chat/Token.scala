package Chat

enum Token:
  case // Terms
       BONJOUR,
       JE,
       ME,
       DE,
       LE,
       SVP,
       ASSOIFFE,
       AFFAME,
  // Price
  PRIX,
  COUTE,
  SOLDE,
  // Question
  COMBIEN,
  QUEL,
  // Politesse
  POLITESSE,
       // Actions
       ETRE,
       VOULOIR,
  APPELLE,
  COMMANDER,
  CONNAITRE,
       // Logic Operators
       ET,
       OU,
       // Products
       PRODUCT,
       PRODUCTS,
       MARQUE,
       // Util
       ETATAME,
  COMMANDE,
  PHRASE,
  IDENTIFICATION,
       PSEUDO,
       NUM,
       EOL,
       UNKNOWN,
       BAD
end Token
