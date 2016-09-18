import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by jcohen66 on 9/16/16.
  */
object PositionService {

  val positions = mutable.Map.empty[String, mutable.SortedSet[Position]]

  def accNetCash(netCash: Double, px: Double, size: Integer, sign: Integer): Double = {
    netCash + (px * size * sign)
  }

  def initNetCash(px: Double, size: Integer, sign: Integer): Double = {
    (px * size * sign)
  }

  def accPosition(position: Integer, size: Integer, sign: Integer): Integer = {
    position + (size * sign)
  }

  def initPosition(px: Double, size: Integer, sign: Integer): Double = {
    (px * size * sign)
  }


  def transact(fill: Fill) = {

      positions.get(fill.symbol) match {
        case Some(positionList) =>

          // Get the last recorded position
          val lastPos = positionList.last


          fill.side match {
            case 'B' =>
              val cashSign = -1
              val posSign = 1
              val pos = Position(fill.symbol, fill.milliseconds, accPosition(lastPos.position, fill.fillSize, posSign), accNetCash(lastPos.netCash, fill.fillPx, fill.fillSize, cashSign)
              )
              positionList.add(pos)


            case 'S' =>
              val cashSign = 1
              val posSign = -1
              val pos = Position(fill.symbol, fill.milliseconds, accPosition(lastPos.position, fill.fillSize, posSign), accNetCash(lastPos.netCash, fill.fillPx, fill.fillSize, cashSign)
              )
              positionList.add(pos)

          }


        case None =>
          var sign = 1
          fill.side match {
            case 'B' =>
              sign = -1
              val pos = Position(fill.symbol, fill.milliseconds, fill.fillSize, initPosition(fill.fillPx, fill.fillSize, sign)
              )
              positions.put(fill.symbol, mutable.SortedSet(pos))

            case 'S' =>
              sign = 1
              val pos = Position(fill.symbol, fill.milliseconds, fill.fillSize, initPosition(fill.fillPx, fill.fillSize, sign)
              )
              positions.put(fill.symbol, mutable.SortedSet(pos))

          }
      }



  }

  def pnl(symbol: String, milliseconds: BigInt, m2m: Double ): Double = {

    position(symbol, milliseconds) match {
      case Some(pos) =>
        "%06.2f".format(((pos.position * m2m) + pos.netCash)).toDouble
      case None =>
        0.00
    }

  }

  def position(symbol: String, milliseconds: BigInt): Option[Position] = positions.get(symbol) match {
    case Some(set) =>
      // Get the last recorded position
      val proximalPositions = set.to(Position("", milliseconds, 0))
      proximalPositions.lastOption

    case None =>
      println(s"No matching position found for ${symbol}")
      None
  }

  def position(symbol: String): Position = {
    positions.get(symbol) match {
      case Some(set) =>
        // Get the last recorded position
        val lastPos = set.last
        lastPos
      case None =>
        println(s"No matching position found for ${symbol}")
        Position(symbol, 0, 0)
    }
  }

  def clear(): Unit = {
    positions.clear
  }
}
