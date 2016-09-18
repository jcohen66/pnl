
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global





/**
  * This is the main entry point to the application.
  *
  * To run from command line:
  *
  *       sbt
  *       run fills prices
  *
  *       Note: Must CTRL-C to end the application run.
  */
object Main extends App {

  override def main(args: Array[String]) {

    if (args.length == 0) {
      println("Usage: Main fills_filename price_filename")
      System.exit(0)
    }

    val arglist = args.toList
    val fills = arglist(0)
    val prices = arglist(1)

    // Use futures to async load the fills data
    Future {
      loadData(fills)
    }.onComplete {
      // Load the prices only after the async load of the fills data completes.
      case Success(result) => loadData(prices)
      case Failure(e) => e.printStackTrace
    }

    while(true) {
      Thread.sleep(1000)
    }
    println("done")

  }

  /**
    * Load and parse the provided files.
    *
    * @param filename
    */

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
    println("CTRL-C to end.")
  }

}
