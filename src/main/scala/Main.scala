import java.math.BigInteger

import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by jcohen66 on 9/16/16.
  */
case class Fill(messageType: String, milliseconds: BigInt, symbol: String, fillPx: Double, fillSize: Integer, side: Char)

case class Px(messageType: String, milliseconds: BigInt, symbol: String, px: Double)

case class Position(symbol: String, milliseconds: BigInt, position: Integer = 0, netCash: Double = 0.0) extends Ordered[Position] {

  // return 0 if the same, negative if this < that, positive if this > that
  def compare(that: Position) = {
    if (this.milliseconds == that.milliseconds)
      0
    else if (this.milliseconds > that.milliseconds)
      1
    else
      -1
  }
}


object Main extends App {

  override def main(args: Array[String]) {

    if (args.length == 0) {
      println("Usage: Main fills_filename price_filename")
      System.exit(0)
    }

    val arglist = args.toList
    val fills = arglist(0)
    val prices = arglist(1)

    Future {
      loadData(fills)
    }.onComplete {
      case Success(result) => loadData(prices)
      case Failure(e) => e.printStackTrace
    }

    println("done")

  }

  def loadData(filename: String) = {
    for (line <- Source.fromFile(filename).getLines) {
      val fields = line.split(" ")
      fields(0) match {
        case "F" =>
          Future {
            val fill = Fill(fields(0), BigInt(fields(1)), fields(2), fields(3).toDouble, fields(4).toInt, fields(5).toCharArray.head)
            PositionService.transact(fill)
          }.onComplete {
            case Success(result) =>
            case Failure(e) => e.printStackTrace
          }

        case "P" =>
          val m2mPx = Px(fields(0), BigInt(fields(1)), fields(2), fields(3).toDouble)
          println(s"PnL for ${m2mPx.symbol} at ${new org.joda.time.DateTime(m2mPx.milliseconds.toLong)} is: " + PositionService.pnl(m2mPx.symbol, m2mPx.milliseconds, m2mPx.px))
        case _ =>
          println("Invalid record type")
      }
    }
  }

}
