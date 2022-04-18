package Chat
import Data.{AccountService, ProductService, Session}
import Utils.Dictionary.dictionary

class AnalyzerService(productSvc: ProductService,
                      accountSvc: AccountService):
  import ExprTree._
  import Data.ProductImpl

  def productImpl : ProductImpl = ProductImpl()
  /**
    * Compute the price of the current node, then returns it. If the node is not a computational node, the method
    * returns 0.0.
    * For example if we had a "+" node, we would add the values of its two children, then return the result.
    * @return the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = t match {
    case And(t1, t2) => computePrice(t1) + computePrice(t2)
    case Or(t1, t2) => math.min(computePrice(t1), computePrice(t2))
    case Order(nb, productName, brandName) => nb * productImpl.getPrice(productName, brandName)
    case _ => 0.0
  }

  /**
    * Build the string describing an order
    * @param t the order to describe
    * @return a description or the order
    * @detail if a "OR" is present in the order, only the less expensive one is described
    */
  def describeOrder(t: ExprTree) : String = t match {
    case And(t1, t2) => describeOrder(t1) + " et " + describeOrder(t2)
    case Or(t1, t2) => if computePrice(t1) <= computePrice(t2) then describeOrder(t1) else describeOrder(t2)
    case Order(nb, productName, brandName) => {
      val brand = dictionary.getOrElse(brandName, productImpl.getDefaultBrand(productName))
      if nb > 1 then s"$nb ${productName}s $brand" else s"$nb ${productName} $brand"
    }
  }

  /**
    * Return the output text of the current node, in order to write it in console.
    * @return the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match
      // TODO - Part 2 Step 3
      // Example cases
      case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case Identification(name) => {
        session.setCurrentUser(name)
        accountSvc.addAccount(name, 30.0)
        s"Bienvenue à vous, $name"
      }
      case Balance() => {
        val currentUser:String = (session.getCurrentUser).getOrElse("")
        val balance:Double = accountSvc.getAccountBalance(currentUser)
        if !balance.isNaN then s"Vous avez ${String.valueOf(balance)} CHF" else "Vous n'avez pas de compte"
      }
      case Order(_,_,_) | And(_,_) | Or(_,_) => {
        val currentUser: String = (session.getCurrentUser).getOrElse("")
        if currentUser.isBlank then "Vous n'avez pas de compte"
        else {
          val price = computePrice(t)
          accountSvc.purchase(currentUser, price)
          val balance: Double = accountSvc.getAccountBalance(currentUser)
          if !balance.isNaN then s"Alors ${describeOrder(t)}, cela coûte $price CHF." +
            s"\nVous avez maintenant ${String.valueOf(balance)} CHF"
          else "Vous n'avez pas de compte"
        }
      }
      case AskPrice(order) => {
        val price = computePrice(order)
        s"Alors pour ${describeOrder(order)}, cela coûterait $price CHF."
      }

end AnalyzerService