package Utils

/**
* Contains the dictionary of the application, which is used to validate, correct and normalize words entered by the
* user.
*/
object Dictionary:
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    "je" -> "je",
    "j" -> "je",
    "me" -> "me",
    "m" -> "me",
    "mon" -> "mon",
    "appelle" -> "appelle",
    "suis" -> "etre",
    "est" -> "etre",
    "veux" -> "vouloir",
    "voudrais" -> "vouloir",
    "aimerais" -> "vouloir",
    "commander" -> "commander",
    "connaitre" -> "connaitre",
    "savoir" -> "connaitre",
    "combien" -> "combien",
    "coûte" -> "coute",
    "coûtent" -> "coute",
    "quel" -> "quel",
    "le" -> "le",
    "prix" -> "prix",
    "de" -> "de",
    "solde" -> "solde",
    "assoiffé" -> "assoiffe",
    "assoiffée" -> "assoiffe",
    "affamé" -> "affame",
    "affamée" -> "affame",
    "bière" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    "et" -> "et",
    "ou" -> "ou",
    "svp" -> "svp",
    "stp" -> "svp",
    "maison" -> "maison",
    "cailler" -> "cailler",
    "farmer" -> "farmer",
    "boxer" -> "boxer",
    "wittekop" -> "wittekop",
    "punkipa" -> "punkipa",
    "jackhammer" -> "jackhammer",
    "ténébreuse" -> "tenebreuse",

  )
end Dictionary
