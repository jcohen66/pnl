
import com.typesafe.scalalogging.{LazyLogging, Logger}
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.file.{Paths, Files}


/**
  * This is the main entry point to the application.
  *
  * To run from command line:
  *
  * sbt <enter>
  * run fills prices
  *
  *   or
  *
  * sbt "run-main Main fills prices"
  *
  * To run tests from the command line:
  *
  * sbt test <enter>
  *
  * Note: Must CTRL-C to end the application run.
  */
object Main extends App with LazyLogging {

  override def main(args: Array[String]) {

    if (args.length == 0) {
      logger.info("Usage: Main fills_filename price_filename")
      System.exit(0)
    }

    val arglist = args.toList
    val fills = arglist(0)
    val prices = arglist(1)

    if(Files.exists(Paths.get(fills)) && Files.exists(Paths.get("prices"))) {

      // Use futures to async load the fills data
      Future {
        loadData(fills)
      }.onComplete {
        // Load the prices only after the async load of the fills data completes.
        case Success(result) => loadData(prices)
        case Failure(e) => e.printStackTrace
      }

      // Simulate a running process allowing the futures to complete.
      while (true) {
        Thread.sleep(1000)
      }
    } else {
      logger.info("\n\n\nOne or both of the input files does not exist.  Cannot proceed.\n")
    }


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

        case Fill.MESSAGE_TYPE =>
          if (fields.length == Fill.REQUIRED_FIELD_COUNT) {
            Future {
              val fill = Fill(fields(Fill.MESSAGE_TYPE_FIELD), BigInt(fields(Fill.MILLISECONDS_FIELD)),
                fields(Fill.SYMBOL_FIELD), fields(Fill.FILL_PX_FIELD).toDouble, fields(Fill.FILL_SIZE_FIELD).toInt,
                fields(Fill.SIDE_FIELD).toCharArray.head)
              PositionService.transact(fill)
            }.onComplete {
              case Success(result) =>
              case Failure(e) => e.printStackTrace
            }
          } else {
            logger.info(s"Skipping ... Invalid fill record format: ${line}")
          }

        case Px.MESSAGE_TYPE =>
          if (fields.length == Px.REQUIRED_FIELD_COUNT) {
            val m2mPx = Px(fields(Px.MESSAGE_TYPE_FIELD), BigInt(fields(Px.MILLISECONDS_FIELD)),
              fields(Px.SYMBOL_FIELD), fields(Px.PX_FIELD).toDouble)
            logger.info(s"PnL for ${m2mPx.symbol} at ${new org.joda.time.DateTime(m2mPx.milliseconds.toLong)} is: " + PositionService.pnl(m2mPx.symbol, m2mPx.milliseconds, m2mPx.px))
          } else {
            logger.info(s"Skipping ... Invalid price record format: ${line}")
          }
        case _ =>
          logger.info(s"Skipping ... Invalid record type: ${line}")
      }
    }
    println("CTRL-C to end.")
  }

}
