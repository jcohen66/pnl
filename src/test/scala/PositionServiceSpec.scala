


import org.scalatest._
import org.scalatest.concurrent.{AsyncAssertions, Eventually, IntegrationPatience}
import scala.collection.mutable


class PositionServiceSpec extends FlatSpec with Matchers with Eventually with AsyncAssertions {


  "PositionService" should "calculate net position" in {

    PositionService.clear()
    val symbol = "MSFT"

    val f_t0 = Fill("F", 1388534400000L, symbol, 42.43, 300, 'B')
    val f_t1 = Fill("F", 1388534638571L, symbol, 42.43, 300, 'B')
    val f_t2 = Fill("F", 1388534839385L, symbol, 42.44, 300, 'B')
    val f_t3 = Fill("F", 1388535240618L, symbol, 42.44, 100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    PositionService.position(symbol).position should be(800)

  }

  "PositionService" should "calculate net position when passed a short" in {

    PositionService.clear()
    val symbol = "MSFT"

    val f_t0 = Fill("F", 1388534400000L, symbol, 42.43, 300, 'B')
    val f_t1 = Fill("F", 1388534638571L, symbol, 42.43, 300, 'B')
    val f_t2 = Fill("F", 1388534839385L, symbol, 42.44, 300, 'B')
    val f_t3 = Fill("F", 1388535240618L, symbol, 42.44, -100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    PositionService.position(symbol).position should be(1000)

  }

  "PositionService" should "calculate net cash" in {

    PositionService.clear()
    val symbol = "MSFT"

    val f_t0 = Fill("F", 1388534400000L, symbol, 42.43, 300, 'B')
    val f_t1 = Fill("F", 1388534638571L, symbol, 42.43, 300, 'B')
    val f_t2 = Fill("F", 1388534839385L, symbol, 42.44, 300, 'B')
    val f_t3 = Fill("F", 1388535240618L, symbol, 42.44, 100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    PositionService.position(symbol).netCash should be((42.43 * 300 * -1) + (42.43 * 300 * -1) + (42.44 * 300 * -1) + (42.44 * 100 * 1))
  }

  "PositionService" should "calculate net P&L when m2m price hasnt chanted" in {

    PositionService.clear()
    val symbol = "MSFT"

    val f_t0 = Fill("F", 1388534400000L, symbol, 42.43, 300, 'B')
    val f_t1 = Fill("F", 1388534638571L, symbol, 42.43, 300, 'B')
    val f_t2 = Fill("F", 1388534839385L, symbol, 42.44, 300, 'B')
    val f_t3 = Fill("F", 1388535240618L, symbol, 42.44, 100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    val p_t0 = Px("P",1388534400002L, symbol, 42.43)

    // val positionByTime = PositionService.position(p_t0.symbol, p_t0.milliseconds)
    // val pnl = "%06.2f".format(((positionByTime.position * p_t0.px) + positionByTime.netCash))
    val pnl = PositionService.pnl(symbol, p_t0.milliseconds, p_t0.px)

    pnl.toDouble should be(0.00)
  }

  "PositionService" should "calculate net P&L when m2m price changes by $1.00" in {

    PositionService.clear()
    val symbol = "MSFT"

    val f_t0 = Fill("F", 1388534400000L, symbol, 42.43, 300, 'B')
    val f_t1 = Fill("F", 1388534638571L, symbol, 42.43, 300, 'B')
    val f_t2 = Fill("F", 1388534839385L, symbol, 42.44, 300, 'B')
    val f_t3 = Fill("F", 1388535240618L, symbol, 42.44, 100, 'S')

    PositionService.transact(f_t0)
    PositionService.transact(f_t1)
    PositionService.transact(f_t2)
    PositionService.transact(f_t3)

    val p_t0 = Px("P",1388534400002L, symbol, 43.43)

    val positionByTime = PositionService.position(p_t0.symbol, p_t0.milliseconds)

    val pnl = PositionService.pnl(symbol, p_t0.milliseconds, p_t0.px)

    pnl.toDouble should be(300.00)
  }

  "A sorted map" should "find closest position items when provided milliseconds not in position server" in {

    val p1 = Position("MSFT", 0, 300)
    val p2 = Position("MSFT", 1, 200)
    val p3 = Position("MSFT", 3, 100)

    val s = mutable.SortedSet(p1, p2, p3)

    val p4 = Position("MSFT", 5, -100)

    s.add(p4)

    s.to(Position("MSFT", 4, 300)).last should be(p3)

  }



  "A timestamp" should "convert to a joda time" in {

    println(new org.joda.time.DateTime(1388534400000L))
    println(new org.joda.time.DateTime(1388534400000L))
    println(new org.joda.time.DateTime(1388538000000L))
    println(new org.joda.time.DateTime(1388538000000L))
    println(new org.joda.time.DateTime(1388541600000L))
    println(new org.joda.time.DateTime(1388541600000L))
    println(new org.joda.time.DateTime(1388545200000L))
    println(new org.joda.time.DateTime(1388545200000L))
    println(new org.joda.time.DateTime(1388548800000L))
    println(new org.joda.time.DateTime(1388548800000L))
    println(new org.joda.time.DateTime(1388552400000L))
    println(new org.joda.time.DateTime(1388552400000L))
    println(new org.joda.time.DateTime(1388556000000L))
    println(new org.joda.time.DateTime(1388556000000L))
    println(new org.joda.time.DateTime(1388559600000L))
    println(new org.joda.time.DateTime(1388559600000L))
  }

  "A SortedSet of positions" should "return the position closest to the millisecond" in {

    val symbol = "MSFT"
    val p1 = Position(symbol, 0, 100)
    val p2 = Position(symbol, 4, 200)

    val positions = mutable.Map.empty[String, mutable.SortedSet[Position]]

    positions.put(symbol, mutable.SortedSet(p1))

    val ss = positions(symbol)
    ss.add(p2)
    println(ss)

    val last = ss.last
    val size = 100
    val p3 = Position(symbol, 6, last.position + size)
    ss.add(p3)

    println(positions)

    ss.to(Position("", 5, 0)).last should be(Position(symbol, 4, 200))
  }
}
