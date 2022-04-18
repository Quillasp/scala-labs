package Chat

/**
  * This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/**
  * Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Etat d'ames
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree
  // Identification
  case class Identification(name:String) extends ExprTree
  // Solde
  case class Balance() extends ExprTree
  // Prix
  case class AskPrice(order: ExprTree) extends ExprTree
  // Commande
  case class Order(nb:Int, productType: String, brandName: String) extends ExprTree
  case class And(t1:ExprTree, t2:ExprTree) extends ExprTree
  case class Or(t1:ExprTree, t2:ExprTree) extends ExprTree
