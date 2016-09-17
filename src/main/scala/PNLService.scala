import scala.collection.mutable

/**
  * Created by jcohen66 on 9/16/16.
  */
object PNLService extends AQRService {
  // private val cache = collection.mutable.Map[String, List[Cash]]()
  private val cache = mutable.LinkedHashMap[String, List[Cash]]()

  val lineItemsBySymbol = mutable.LinkedHashMap.empty[String, mutable.LinkedHashMap[BigInt, Cash]]

  lineItemsBySymbol.getOrElseUpdate("", mutable.LinkedHashMap.empty[BigInt, Cash] )


  def pnl(symbol: String, milliseconds: BigInt ): Cash = {
    lineItemsBySymbol(symbol)(milliseconds)
  }

  override def transact(fill: Fill) = {

    var sign =
      fill.side match {
        case 'S' => -1
        case _ => 1
      }
    val cash = cache.getOrElse(fill.symbol, List())
    val tcash = fill.fillPx * fill.fillSize * sign
    val entry = Cash(fill.symbol,
      fill.milliseconds,
      tcash,
      PNLService.entries(fill.symbol).foldLeft(0.0)(_ + _.cash) + tcash
    )
    val list = cash ::: List(entry)
    cache.put(fill.symbol, cash ::: List(entry))
  }

  def entries(symbol: String): List[Cash] = {
    lineItemsBySymbol(symbol).values.toList
  }

  def transact2(fill: Fill) = {

    val lineItems = lineItemsBySymbol.getOrElseUpdate(fill.symbol, mutable.LinkedHashMap.empty[BigInt, Cash] )
      .values.filter{x => x.milliseconds < fill.milliseconds}

    val sign =
      fill.side match {
        case 'S' => -1
        case _ => 1
      }
    val cash = cache.getOrElse(fill.symbol, List())
    val tcash = fill.fillPx * fill.fillSize * sign
    val entry = Cash(fill.symbol,
      fill.milliseconds,
      tcash,
      lineItems.foldLeft(0.0)(_ + _.cash) + tcash
    )
    lineItemsBySymbol(fill.symbol)(fill.milliseconds) = entry
  }

}
