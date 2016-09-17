import scala.collection.mutable

/**
  * Created by jcohen66 on 9/16/16.
  */
object PositionService extends AQRService {


  val positions = mutable.Map.empty[String, Option[mutable.SortedSet[Position]]]


  def transact(fill: Fill) = {

    positions.get(fill.symbol) match {
      case Some(p) =>
        // Get the last recorded position
        val lastPos = p.get.last
        var sign = 1
        fill.side match {
          case 'B' =>
            sign = -1
            val pos = Position(
              fill.symbol,
              fill.milliseconds,
              lastPos.position + (fill.fillSize * sign),
              lastPos.netCash + (fill.fillPx * fill.fillSize * sign)
            )
            p.get.add(pos)

          case 'S' =>
            sign = 1
            val pos = Position(
              fill.symbol,
              fill.milliseconds,
              lastPos.position + (fill.fillSize * sign),
              lastPos.netCash + (fill.fillPx * fill.fillSize * sign)
            )
            p.get.add(pos)
        }


      case None =>
        var sign = 1
        fill.side match {
          case 'B' =>
            sign = -1
            val pos = Position(
              fill.symbol,
              fill.milliseconds,
              fill.fillSize,
              (fill.fillPx * fill.fillSize * sign)
            )
            positions.put(fill.symbol, Some(mutable.SortedSet(pos)))
          case 'S' =>
            sign = 1
            val pos = Position(
              fill.symbol,
              fill.milliseconds,
              fill.fillSize,
              (fill.fillPx * fill.fillSize * sign)
            )
            positions.put(fill.symbol, Some(mutable.SortedSet(pos)))
        }

    }

  }


  def position(symbol: String, milliseconds: BigInt): Position = {
    positions.get(symbol) match {
      case Some(p) =>
        // Get the last recorded position
        p match {
          case Some(set) =>
            val lastPos = set.to(Position("", milliseconds, 0)).last
            lastPos
          case None =>
            println(s"No matching position found for ${symbol} at ${milliseconds}")
            Position(symbol, milliseconds, 0)
        }
      case None =>
    }

  }

  def position(symbol: String): Position = {
    positions.get(symbol) match {
      case Some(p) =>
        // Get the last recorded position
        p match {
          case Some(set) =>
            val lastPos = set.last
            lastPos
          case None =>
            println(s"No matching position found for ${symbol}")
            Position(symbol, 0, 0)
        }
    }
  }
}
