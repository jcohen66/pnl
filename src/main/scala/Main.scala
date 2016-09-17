import java.math.BigInteger

import scala.io.Source

/**
  * Created by jcohen66 on 9/16/16.
  */
case class Fill(messageType: String, milliseconds: BigInt, symbol: String, fillPx: Double, fillSize: Integer, side: Char)

case class Px(messageType: String, milliseconds: BigInt, symbol: String, px: Double)

case class Position(symbol: String, milliseconds: BigInt, position: Integer = 0, netCash: Double = 0.0) extends Ordered[Position] {

  // return 0 if the same, negative if this < that, positive if this > that
  def compare (that: Position) = {
    if (this.milliseconds == that.milliseconds)
      0
    else if (this.milliseconds > that.milliseconds)
      1
    else
      -1
  }
}

case class Cash(symbol: String, milliseconds: BigInt, cash: Double, netCash: Double)

object Main extends App {

  override def main(args: Array[String]) {

    if (args.length == 0) {
      println("Usage: Main fills_filename price_filename")
      System.exit(0)
    }

    val arglist = args.toList
    val fills = arglist(0)
    val prices = arglist(1)

    loadData(fills)
    loadData(prices)
    println("done")

  }

  def loadData(filename: String) = {
    for (line <- Source.fromFile(filename).getLines) {
      val fields = line.split(" ")
      fields(0) match {
        case "F" =>
          val fill = Fill(fields(0), BigInt(fields(1)), fields(2), fields(3).toDouble, fields(4).toInt, fields(5).toCharArray.head)
          PositionService.transact(fill)
          // PNLService.transact2(fill)
          // println(new org.joda.time.DateTime(fill.milliseconds.toLong))
        case "P" =>
          val price = Px(fields(0), BigInt(fields(1)), fields(2), fields(3).toDouble)
          val posm = PositionService.position(price.symbol, price.milliseconds)
          println(s"PnL for ${price.symbol} at ${new org.joda.time.DateTime(price.milliseconds.toLong)} is: ${((posm.position * price.px) - posm.netCash)}")
        case _ =>
          println("Invalid record type")
      }
      // println(line)
    }
    println(PositionService.position("MSFT"))
    println(PositionService.positions("MSFT"))
  }

}
