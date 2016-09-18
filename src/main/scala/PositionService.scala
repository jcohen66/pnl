import scala.collection.mutable

case class Fill(messageType: String, milliseconds: BigInt, symbol: String, fillPx: Double, fillSize: Integer, side: Char)
case class Px(messageType: String, milliseconds: BigInt, symbol: String, px: Double)

/**
  * Our position cache must contain positions for each symbol ordered by the timestamp so
  * that we can hash into the position at a point in time.  Need a comparator on milliseconds.
  *
  *
  * @param symbol
  * @param milliseconds
  * @param position
  * @param netCash
  */
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

object PositionService {

  /**
    * Use a TreeSet for position line items so they will be sorted by milliseconds
    * within each instrument.  SortedSet is the Scala TreeSet implementation.
    * It provides the ability to lookup the closest millisecond even if an exact
    * match is not present.
    */
  val positions = mutable.Map.empty[String, mutable.SortedSet[Position]]


  /**
    * Net Cash accumulator calculates flow of cash.
    *
    * Isolate the calculation so it can be tested in isolation.
    *
    * @param netCash
    * @param px
    * @param size
    * @param sign
    * @return
    */
  def accNetCash(netCash: Double, px: Double, size: Integer, sign: Integer): Double = {
    netCash + (px * size * sign)
  }

  /**
    * Net Cash Initializer
    *
    * Isolate the calculation so it can be tested in isolation.
    *
    *
    * @param px
    * @param size
    * @param sign
    * @return
    */
  def initNetCash(px: Double, size: Integer, sign: Integer): Double = {
    (px * size * sign)
  }

  /**
    * Position accumulator.  Because it takes so much time to calculate from
    * start each time, roll accumulate the position on each fill.
    *
    * Isolate the calculation so it can be tested in isolation.
    *
    *
    * @param position
    * @param size
    * @param sign
    * @return
    */
  def accPosition(position: Integer, size: Integer, sign: Integer): Integer = {
    position + (size * sign)
  }


  /**
    * Isolate the calculation so it can be tested in isolation.
    *
    * @param px
    * @param size
    * @param sign
    * @return
    */
  def initPosition(px: Double, size: Integer, sign: Integer): Double = {
    (px * size * sign)
  }


  /**
    * This is the workhorse method.  Loads a transaction into the positions cache
    * preserving the insert order for later lookup of milliseconds.
    *
    *
    * @param fill
    * @return
    */
  def transact(fill: Fill) = {

      positions.get(fill.symbol) match {
        case Some(positionList) =>

          /**
            * Get the last recorded position since it will have the accumulated
            * position calculated, just need to apply the current transaction.
            */
          val lastPos = positionList.last

          /**
            * Updates to cache.
            */
          fill.side match {
            /**
              * Buy
              */
            case 'B' =>
              val cashSign = -1
              val posSign = 1
              val pos = Position(fill.symbol, fill.milliseconds, accPosition(lastPos.position, fill.fillSize, posSign), accNetCash(lastPos.netCash, fill.fillPx, fill.fillSize, cashSign)
              )
              positionList.add(pos)

            /**
              * Standard sell
              */
            case 'S' if fill.fillSize >= 0 =>
              val cashSign = 1
              val posSign = -1
              val pos = Position(fill.symbol, fill.milliseconds, accPosition(lastPos.position, fill.fillSize, posSign), accNetCash(lastPos.netCash, fill.fillPx, fill.fillSize, cashSign)
              )
              positionList.add(pos)

            /**
              * Short sell
              */
            case 'S' if fill.fillSize < 0 =>
              val cashSign = -1
              val posSign = -1
              val pos = Position(fill.symbol, fill.milliseconds, accPosition(lastPos.position, fill.fillSize, posSign), accNetCash(lastPos.netCash, fill.fillPx, fill.fillSize, cashSign)
              )
              positionList.add(pos)

          }


        /**
          * Initial entries to cache.
          */
        case None =>

          fill.side match {
            /**
              * Buy
              */
            case 'B' =>
              val sign = -1
              val pos = Position(fill.symbol, fill.milliseconds, fill.fillSize, initPosition(fill.fillPx, fill.fillSize, sign)
              )
              positions.put(fill.symbol, mutable.SortedSet(pos))

            /**
              * Regular Sell
              */
            case 'S' if fill.fillSize >= 0 =>
              val sign = 1
              val pos = Position(fill.symbol, fill.milliseconds, fill.fillSize, initPosition(fill.fillPx, fill.fillSize, sign)
              )
              positions.put(fill.symbol, mutable.SortedSet(pos))

            /**
              * Short sell
              */
            case 'S' if fill.fillSize < 0 =>
              val sign = -1
              val pos = Position(fill.symbol, fill.milliseconds, fill.fillSize, initPosition(fill.fillPx, fill.fillSize, sign)
              )
              positions.put(fill.symbol, mutable.SortedSet(pos))


          }
      }



  }

  /**
    * This isolates the calculation so it can be tested independently
    *
    *
    * @param symbol
    * @param milliseconds
    * @param m2m
    * @return
    */
  def pnl(symbol: String, milliseconds: BigInt, m2m: Double ): Double = {

    position(symbol, milliseconds) match {
      case Some(pos) =>
        "%06.2f".format(((pos.position * m2m) + pos.netCash)).toDouble
      case None =>
        0.00
    }

  }

  /**
    * Lookup the entry in time in the cache.
    *
    * @param symbol
    * @param milliseconds
    * @return
    */
  def position(symbol: String, milliseconds: BigInt): Option[Position] = positions.get(symbol) match {
    case Some(set) =>
      // Get the last recorded position
      val proximalPositions = set.to(Position("", milliseconds, 0))
      proximalPositions.lastOption

    case None =>
      println(s"No matching position found for ${symbol}")
      None
  }

  /**
    * Get the most current position from the cache.
    *
    *
    * @param symbol
    * @return
    */
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

  /**
    * Clear the cache.
    *
    */
  def clear(): Unit = {
    positions.clear
  }
}
