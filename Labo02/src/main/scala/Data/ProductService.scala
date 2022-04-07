package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // TODO - Part 2 Step 2
  def getPrice(product: ProductName, brand: String): Double = product match
    case "biere" =>
      brand match
        case "farmer" => 1.0
        case "wittekop" => 2.0
        case "punkipa" => 3.0
        case "jackhammer" => 3.0
        case "tenebreuse" => 4.0
        case _ => 1.0
    case "croissant" =>
      brand match
        case "cailler" => 2.0
        case _ => 2.0



  def getDefaultBrand(product: ProductName): BrandName = product match
    case "biere" => "boxer"
    case "croissant" => "maison"
end ProductImpl
